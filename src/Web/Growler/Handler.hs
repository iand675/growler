{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Growler.Handler where
import           Control.Applicative
import           Control.Lens
import           Control.Monad.RWS
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import qualified Control.Monad.State as State
import qualified Control.Monad.State.Strict as ST
import           Data.Aeson                hiding ((.=))
import qualified Data.ByteString.Char8     as C
import           Data.CaseInsensitive
import           Data.Maybe
import qualified Data.HashMap.Strict       as HM
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Text as T
import           Data.Text.Encoding as T
import           Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Parse hiding (Param)
import           Network.HTTP.Types
import           Web.Growler.Types hiding (status, request, params)
import qualified Web.Growler.Types as L
import Pipes.Wai
import Pipes.Aeson

initialState :: ResponseState
initialState = ResponseState ok200 HM.empty (LBSSource "")

currentResponse :: Monad m => HandlerT m ResponseState
currentResponse = HandlerT State.get

abort :: Monad m => ResponseState -> HandlerT m ()
abort rs = HandlerT $ lift $ left rs

status :: Monad m => Status -> HandlerT m ()
status v = HandlerT $ L.status .= v

addHeader :: Monad m => CI C.ByteString -> C.ByteString -> HandlerT m ()
addHeader k v = HandlerT (L.headers %= HM.insertWith (\_ v' -> v:v') k [v])

setHeader :: Monad m => CI C.ByteString -> C.ByteString -> HandlerT m ()
setHeader k v = HandlerT (L.headers %= HM.insert k [v])

body :: Monad m => BodySource -> HandlerT m ()
body = HandlerT . (bodySource .=)

json :: Monad m => ToJSON a => a -> HandlerT m ()
json x = do
  body $ LBSSource $ encode x
  addHeader "Content-Type" "application/json"

file :: Monad m => FilePath -> Maybe FilePart -> HandlerT m ()
file fpath fpart = HandlerT (bodySource .= FileSource (fpath, fpart))

formData :: MonadIO m => BackEnd y -> HandlerT m ([(C.ByteString, C.ByteString)], [File y])
formData b = do
  r <- request
  liftIO $ parseRequestBody b r

-- header :: Monad m => CI ByteString -> ByteString -> HandlerT m ()

headers :: Monad m => HandlerT m RequestHeaders
headers = liftM requestHeaders request

jsonData :: (FromJSON a, MonadIO m) => HandlerT m (Either String a)
jsonData = do
  r <- request
  ejs <- ST.evalStateT Pipes.Aeson.decode $ producerRequestBody r
  return $! case ejs of
    Nothing -> Left "Request body exhausted while parsing JSON"
    Just res -> case res of
      Left err -> Left $! case err of
        AttoparsecError err -> show err
        FromJSONError err -> err
      Right r -> Right r

-- param :: 

params :: Monad m => HandlerT m [Param]
params = HandlerT (view L.params)

raw :: Monad m => L.ByteString -> HandlerT m ()
raw bs = HandlerT (bodySource .= LBSSource bs)

redirect :: Monad m => T.Text -> HandlerT m ()
redirect url = do
  status found302
  setHeader "Location" $ T.encodeUtf8 url
  currentResponse >>= abort

request :: Monad m => HandlerT m Request
request = HandlerT $ view $ L.request

stream :: Monad m => StreamingBody -> HandlerT m ()
stream s = HandlerT (bodySource .= StreamSource s)

text :: Monad m => TL.Text -> HandlerT m ()
text t = do
  setHeader hContentType "text/plain; charset=utf-8"
  raw $ TL.encodeUtf8 t

html :: Monad m => TL.Text -> HandlerT m ()
html t = do
  setHeader hContentType "text/html; charset=utf-8"
  raw $ TL.encodeUtf8 t

routePattern :: Monad m => HandlerT m (Maybe T.Text)
routePattern = HandlerT $ view $ L.matchedPattern

runHandler :: Monad m => ResponseState -> Maybe T.Text -> Request -> [Param] -> HandlerT m a -> m (Either ResponseState (a, ResponseState))
runHandler rs pat rq ps m = runEitherT $ do
  (dx, r, ()) <- runRWST (fromHandler m) (RequestState pat (qsParams ++ ps) rq) rs
  return (dx, r)
  where
    qsParams = fmap (_2 %~ fromMaybe "") (queryString rq) 

liftAround :: (Monad m) => (forall a. m a -> m a) -> HandlerT m a -> HandlerT m a
liftAround f m = HandlerT $ do
  (RequestState pat ps req) <- ask
  currentState <- get
  r <- lift $ lift $ f $ runHandler currentState pat req ps m
  case r of
    Left err -> lift $ left err
    Right (dx, state') -> do
      put state'
      return dx 
