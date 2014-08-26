{-# LANGUAGE OverloadedStrings #-}
module Web.Growler.Handler where
import           Control.Applicative
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.RWS
import           Control.Monad.Trans
import qualified Control.Monad.State as State
import qualified Control.Monad.State.Strict as ST
import           Data.Aeson                hiding ((.=))
import qualified Data.ByteString.Char8     as C
import Data.CaseInsensitive
import Data.Maybe
import qualified Data.HashMap.Strict       as HM
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Network.HTTP.Types.Status
import           Network.Wai
import Network.Wai.Parse hiding (Param)
import Network.HTTP.Types
import           Web.Growler.Types
import Pipes.Wai
import Pipes.Aeson

initialState :: ResponseState
initialState = (ok200, HM.empty, LBSSource "")

currentResponse :: Monad m => HandlerT m ResponseState
currentResponse = HandlerT State.get

abort :: Monad m => ResponseState -> HandlerT m ()
abort rs = do
  (q, _, _) <- HandlerT ask
  HandlerT $ lift $ q rs

status :: Monad m => Status -> HandlerT m ()
status v = HandlerT $ _1 .= v

addHeader :: Monad m => CI C.ByteString -> C.ByteString -> HandlerT m ()
addHeader k v = HandlerT (_2 %= HM.insertWith (\_ v' -> v:v') k [v])

setHeader :: Monad m => CI C.ByteString -> C.ByteString -> HandlerT m ()
setHeader k v = HandlerT (_2 %= HM.insert k [v])

body :: Monad m => BodySource -> HandlerT m ()
body = HandlerT . (_3 .=)

json :: Monad m => ToJSON a => a -> HandlerT m ()
json = body . LBSSource . encode

file :: Monad m => FilePath -> Maybe FilePart -> HandlerT m ()
file fpath fpart = HandlerT (_3 .= FileSource (fpath, fpart))

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
params = HandlerT (view _3)

raw :: Monad m => L.ByteString -> HandlerT m ()
raw bs = HandlerT (_3 .= LBSSource bs)

redirect :: Monad m => T.Text -> HandlerT m ()
redirect url = do
  status found302
  setHeader "Location" $ T.encodeUtf8 url
  currentResponse >>= abort

request :: Monad m => HandlerT m Request
request = HandlerT $ view _2

stream :: Monad m => StreamingBody -> HandlerT m ()
stream s = HandlerT (_3 .= StreamSource s)

text :: Monad m => TL.Text -> HandlerT m ()
text t = do
  setHeader hContentType "text/plain; charset=utf-8"
  raw $ TL.encodeUtf8 t

html :: Monad m => TL.Text -> HandlerT m ()
html t = do
  setHeader hContentType "text/html; charset=utf-8"
  raw $ TL.encodeUtf8 t

runHandler :: Monad m => ResponseState -> Request -> [Param] -> HandlerT m a -> m ResponseState
runHandler rs rq ps m = runContT runInner return
  where
    qsParams = fmap (_2 %~ fromMaybe "") (queryString rq) 
    runInner = callCC $ \e -> fst <$> execRWST (fromHandler m >> Control.Monad.RWS.get) (e, rq, qsParams ++ ps) rs
