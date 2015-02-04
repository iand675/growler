{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
module Web.Growler.Types where
import           Blaze.ByteString.Builder  (Builder)
import           Control.Applicative
import           Control.Exception
import           Control.Lens.TH
import           Control.Monad.Base (MonadBase(..), liftBaseDefault)
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Control
import           Data.Aeson                  hiding ((.=))
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as L
import qualified Data.CaseInsensitive        as CI
import qualified Data.HashMap.Strict         as HM
import           Data.Monoid
import           Data.String                 (IsString (..))
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai
import           Pipes.Aeson                 (DecodingError (..))

data MatchResult = Fail | Partial [Param] | Complete [Param]
  deriving (Show, Eq)

newtype RoutePattern = RoutePattern { runRoutePattern :: Request -> RoutePatternResult }

data RoutePatternResult = RoutePatternResult
  { routePatternResultName        :: !Text
  , routePatternResultRequest     :: !Request     -- ^ The (potentially) updated request after consuming a portion of the path
  , routePatternResultMatchResult :: !MatchResult
  }

instance Monoid MatchResult where
  mappend l r = case l of
    Fail -> Fail
    Partial lps -> case r of
      Fail -> Fail
      Partial rps -> Partial (lps <> rps)
      Complete rps -> Complete (lps <> rps)
    Complete lps -> case r of
      Complete rps -> Complete (lps <> rps)
      _ -> Fail
  mempty = Partial []

instance Monoid RoutePattern where
  mappend (RoutePattern a) (RoutePattern b) = RoutePattern $ \r -> let (RoutePatternResult t1 r' p1) = a r in
                                                                   let (RoutePatternResult t2 r'' p2) = b r' in
                                                                     RoutePatternResult (t1 <> t2) r'' (p1 <> p2)
  mempty = RoutePattern $ \r -> RoutePatternResult "" r $ Partial []

instance IsString RoutePattern where
  fromString = capture . T.pack

path :: Request -> T.Text
path r = case front of
  Just ('/', _) -> full
  _ -> T.cons '/' full
  where
    full = T.intercalate "/" $ pathInfo r
    front = T.uncons full

capture :: Text -> RoutePattern
capture pat = RoutePattern process
  where 
    process req = RoutePatternResult pat (req { pathInfo = ss }) res
      where
        (res, ss) = go (T.split (== '/') pat) (T.split (== '/') $ path req) []
        go [] [] prs = (Complete prs, []) -- request string and pattern match!
        go [] r  prs | T.null (mconcat r)  = (Complete prs, []) -- in case request has trailing slashes
                     | otherwise           = (Partial prs, r)  -- request string is longer than pattern
        go p  [] prs | T.null (mconcat p)  = (Complete prs, []) -- in case pattern has trailing slashes
                     | otherwise           = (Fail, [])         -- request string is not long enough
        go (p:ps) (r:rs) prs | p == r          = go ps rs prs -- equal literals, keeping checking
                             | T.null p        = (Fail, [])         -- p is null, but r is not, fail
                             | T.head p == ':' = go ps rs $ (T.encodeUtf8 $ T.tail p, T.encodeUtf8 r) : prs -- p is a capture, add to params
                             | otherwise       = (Fail, [])     -- both literals, but unequal, fail

type Param = (C.ByteString, C.ByteString)

data BodySource = FileSource !FilePath !(Maybe FilePart)
                | BuilderSource !Builder
                | LBSSource !L.ByteString
                | StreamSource !StreamingBody
                | RawSource !(IO C.ByteString -> (C.ByteString -> IO ()) -> IO ()) !Response

data RequestState = RequestState
  { requestStateMatchedPattern :: Maybe T.Text
  , requestStateParams         :: [Param]
  , requestStateRequest        :: Request
  }

data ResponseState = ResponseState
  { responseStateStatus     :: !Status
  , responseStateHeaders    :: !(HM.HashMap (CI.CI C.ByteString) [C.ByteString])
  , responseStateBodySource :: !BodySource
  }

makeFields ''ResponseState
makeFields ''RequestState

type EarlyTermination = ResponseState
type HandlerAbort m = EitherT EarlyTermination m
newtype HandlerT m a = HandlerT
  { fromHandler :: RWST RequestState () ResponseState (HandlerAbort m) a
  } deriving (Functor, Monad, Applicative)

instance MonadTrans HandlerT where
  lift m = HandlerT $ lift $ lift m

deriving instance MonadIO m => MonadIO (HandlerT m)

instance MonadBase b m => MonadBase b (HandlerT m) where
  liftBase = liftBaseDefault

newtype StHandlerT a = StHandlerT { unStHandlerT :: Either ResponseState (a, ResponseState) }

instance MonadTransControl HandlerT where
  type StT HandlerT a = StHandlerT a
  liftWith f = do
    r <- HandlerT ask
    s <- HandlerT get
    lift $ f $ \h -> do
      res <- runEitherT $ runRWST (fromHandler h) r s
      return $ StHandlerT $ case res of
        Left s -> Left s
        Right (x, s, _) -> Right (x, s)

  restoreT mSt = HandlerT $ do
    (StHandlerT stof) <- lift $ lift $ mSt
    case stof of
      Left s -> do
        put s
        lift $ left s
      Right (x, s) -> do
        put s
        return x

instance MonadBaseControl b m => MonadBaseControl b (HandlerT m) where
  type StM (HandlerT m) a = ComposeSt HandlerT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

type Handler = HandlerT IO

newtype GrowlerT m a = GrowlerT { fromGrowlerT :: StateT [(StdMethod, RoutePattern, HandlerT m ())] m a }

instance Functor m => Functor (GrowlerT m) where
  fmap f (GrowlerT m) = GrowlerT (fmap f m)

instance (Functor m, Monad m) => Applicative (GrowlerT m) where
  pure = GrowlerT . pure
  (GrowlerT f) <*> (GrowlerT r) = GrowlerT (f <*> r)

deriving instance Monad m => Monad (GrowlerT m)

instance MonadIO m => MonadIO (GrowlerT m) where
  liftIO = GrowlerT . liftIO

type Growler = GrowlerT IO

data JsonInputError = RequestBodyExhausted
                    | JsonError DecodingError
                    deriving (Show, Eq)

data GrowlerConfig m = GrowlerConfig
  { growlerConfigNotFoundHandler :: HandlerT m () -- ^ The 404 not found handler. If no route matches, then this handler will be evaluated.
  , growlerConfigErrorHandler    :: SomeException -> HandlerT m () -- ^ The uncaught exception handler. If an exception is thrown and not caught while trying to service a request, then this handler will be evaluated.
  }

