{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
module Web.Growler.Types where
import           Blaze.ByteString.Builder  (Builder)
import           Control.Applicative
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
import           Data.String                 (IsString (..))
import           Data.Text                   (Text, pack)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai

data RoutePattern = Capture  Text
                  | Literal  Text
                  | Function (Request -> Text) (Request -> Maybe [Param])

instance IsString RoutePattern where
    fromString = Capture . pack

type Param = (C.ByteString, C.ByteString)

data BodySource = FileSource !(FilePath, Maybe FilePart)
                | BuilderSource !Builder
                | LBSSource !L.ByteString
                | StreamSource !StreamingBody
                | RawSource !(IO C.ByteString -> (C.ByteString -> IO ()) -> IO ()) !Response

data RequestState = RequestState
  { requestMatchedPattern :: Maybe RoutePattern
  , requestParams         :: [Param]
  , requestRequest        :: Request
  }

data ResponseState = ResponseState
  { responseStatus     :: !Status
  , responseHeaders    :: !(HM.HashMap (CI.CI C.ByteString) [C.ByteString])
  , responseBodySource :: !BodySource
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

instance MonadTransControl HandlerT where
  newtype StT HandlerT a = StHandlerT { unStHandlerT :: Either ResponseState (a, ResponseState) }
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
  newtype StM (HandlerT m) a = StMHandlerT { unStMHandlerT :: StM m (StT HandlerT a) }
  liftBaseWith = defaultLiftBaseWith StMHandlerT
  restoreM = defaultRestoreM unStMHandlerT

type Handler = HandlerT IO

newtype GrowlerT m a = GrowlerT { fromGrowlerT :: StateT [(StdMethod, RoutePattern, HandlerT m ())] m a}

instance Functor m => Functor (GrowlerT m) where
  fmap f (GrowlerT m) = GrowlerT (fmap f m)

instance (Functor m, Monad m) => Applicative (GrowlerT m) where
  pure = GrowlerT . pure
  (GrowlerT f) <*> (GrowlerT r) = GrowlerT (f <*> r)

deriving instance Monad m => Monad (GrowlerT m)

instance MonadIO m => MonadIO (GrowlerT m) where
  liftIO = GrowlerT . liftIO

type Growler = GrowlerT IO

