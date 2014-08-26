{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Web.Growler.Types where
import           Blaze.ByteString.Builder  (Builder)
import           Control.Applicative
import           Control.Monad.Cont
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Aeson                hiding ((.=))
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as L
import qualified Data.CaseInsensitive      as CI
import qualified Data.HashMap.Strict       as HM
import           Data.String               (IsString (..))
import           Data.Text                 (Text, pack)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai

data BodySource = FileSource !(FilePath, Maybe FilePart)
                | BuilderSource !Builder
                | LBSSource !L.ByteString
                | StreamSource !StreamingBody
                | RawSource !(IO C.ByteString -> (C.ByteString -> IO ()) -> IO ()) !Response

type ResponseState = (Status, HM.HashMap (CI.CI C.ByteString) [C.ByteString], BodySource)

type HandlerAbort m = ContT ResponseState m
newtype HandlerT m a = HandlerT
  { fromHandler :: RWST (ResponseState -> HandlerAbort m (), Request, [Param]) () ResponseState (HandlerAbort m) a
  }

instance Functor m => Functor (HandlerT m) where
  fmap f (HandlerT m) = HandlerT (fmap f m)

instance Applicative m => Applicative (HandlerT m) where
  pure = HandlerT . pure
  (HandlerT f) <*> (HandlerT r) = HandlerT (f <*> r)

deriving instance Monad m => Monad (HandlerT m)

instance MonadTrans HandlerT where
  lift m = HandlerT $ lift $ lift m

deriving instance MonadIO m => MonadIO (HandlerT m)

type Handler = HandlerT IO

type Param = (C.ByteString, C.ByteString)
data RoutePattern = Capture   Text
                  | Literal   Text
                  | Function  (Request -> Maybe [Param])

instance IsString RoutePattern where
    fromString = Capture . pack

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
