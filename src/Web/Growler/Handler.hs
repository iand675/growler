{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Growler.Handler where
import           Blaze.ByteString.Builder         (Builder)
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import qualified Control.Monad.Trans.RWS.Strict   as RWS
import qualified Control.Monad.Trans.State.Strict as ST
import           Data.Aeson                       hiding ((.=))
import qualified Data.ByteString.Char8            as C
import qualified Data.ByteString.Lazy.Char8       as L
import           Data.CaseInsensitive
import           Data.Maybe
import           Data.Monoid                      ((<>))
import qualified Data.HashMap.Strict              as HM
import           Data.Text                        as T
import           Data.Text.Encoding               as T
import           Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Encoding          as TL
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Parse                hiding (Param)
import           Network.HTTP.Types
import           Web.Growler.Parsable
import           Web.Growler.Types                hiding (status, request, params)
import qualified Web.Growler.Types                as L
import           Pipes.Wai
import           Pipes.Aeson

initialState :: ResponseState
initialState = ResponseState ok200 HM.empty (LBSSource "")

currentResponse :: Monad m => HandlerT m ResponseState
currentResponse = HandlerT RWS.get

-- | End the handler early with an arbitrary 'ResponseState'. 
abort :: Monad m => ResponseState -> HandlerT m ()
abort rs = HandlerT $ lift $ left rs

-- | Set the response status code.
status :: Monad m => Status -> HandlerT m ()
status v = HandlerT $ L.status .= v

-- | Add a header to the response. Header names are case-insensitive.
addHeader :: Monad m => CI C.ByteString -> C.ByteString -> HandlerT m ()
addHeader k v = HandlerT (L.headers %= HM.insertWith (\_ v' -> v:v') k [v])

-- | Set a response header. Overrides duplicate headers of the same name.
setHeader :: Monad m => CI C.ByteString -> C.ByteString -> HandlerT m ()
setHeader k v = HandlerT (L.headers %= HM.insert k [v])

-- | Set an arbitrary body source for the response.
body :: Monad m => BodySource -> HandlerT m ()
body = HandlerT . (bodySource .=)

-- | Send a file as the response body.
file :: Monad m => FilePath       -- ^ The file to send
                -> Maybe FilePart -- ^ If 'Nothing', then send the whole file, otherwise, the part specified 
                -> HandlerT m ()
file fpath fpart = HandlerT (bodySource .= FileSource fpath fpart)

-- | Set the response body to a ByteString 'Builder'. Sets no headers.
builder :: Monad m => Builder
                   -> HandlerT m ()
builder b = HandlerT (bodySource .= BuilderSource b)

-- | Set the response body to a lazy 'ByteString'. Sets no headers.
bytestring :: Monad m => L.ByteString -> HandlerT m ()
bytestring bs = HandlerT (bodySource .= LBSSource bs)

-- | Send a streaming response body. Sets no headers.
stream :: Monad m => StreamingBody -> HandlerT m ()
stream s = HandlerT (bodySource .= StreamSource s)

-- | Send raw output as the response body. Useful for e.g. websockets. See WAI's @responseRaw@ for more details.
raw :: MonadIO m => (IO C.ByteString -> (C.ByteString -> IO ()) -> IO ())
                 -> Response -- ^ Backup response when the WAI provider doesn't support upgrading (e.g. CGI)
                 -> HandlerT m ()
raw f r = HandlerT (bodySource .= RawSource f r)

-- | Send a value as JSON as the response body. Also sets the content type to application/json.
json :: Monad m => ToJSON a => a -> HandlerT m ()
json x = do
  body $ LBSSource $ encode x
  addHeader "Content-Type" "application/json"

-- | Parse out the form parameters and the uploaded files. Consumes the request body.
formData :: MonadIO m => BackEnd y -> HandlerT m ([(C.ByteString, C.ByteString)], [File y])
formData b = do
  r <- request
  liftIO $ parseRequestBody b r

-- | Get all the request headers.
headers :: Monad m => HandlerT m RequestHeaders
headers = liftM requestHeaders request

-- | Consume the request body as a JSON value. Returns a 'JsonInputError' on failure.
jsonData :: (FromJSON a, MonadIO m) => HandlerT m (Either JsonInputError a)
jsonData = do
  r <- request
  ejs <- ST.evalStateT Pipes.Aeson.decode $ producerRequestBody r
  return $! case ejs of
    Nothing -> Left RequestBodyExhausted
    Just res -> case res of
      Left err -> Left $ JsonError err
      Right r -> Right r

-- | Get all matched params.
params :: Monad m => HandlerT m [Param]
params = HandlerT (view L.params)

-- | Terminate the current handler and send a @302 Found@ redirect to the provided URL.
-- Other headers that have already been set will also be returned in the request.
redirect :: Monad m => T.Text -- ^ URL to redirect to.
                    -> HandlerT m ()
redirect url = do
  status found302
  setHeader "Location" $ T.encodeUtf8 url
  currentResponse >>= abort

-- | Get the underlying WAI 'Request'
request :: Monad m => HandlerT m Request
request = HandlerT $ view $ L.request

-- | Return plain text as the response body. Sets the Content-Type header to \"text/plain; charset=utf-8\".
text :: Monad m => TL.Text -> HandlerT m ()
text t = do
  setHeader hContentType "text/plain; charset=utf-8"
  bytestring $ TL.encodeUtf8 t

-- | Return HTML as the response body. Sets the Content-Type header to \"text/html; charset=utf-8\".
-- If you're using something like blaze-html or lucid, you'll probably get better performance by rolling
-- your own function that sets the response body to a 'Builder'.
html :: Monad m => TL.Text -> HandlerT m ()
html t = do
  setHeader hContentType "text/html; charset=utf-8"
  bytestring $ TL.encodeUtf8 t

-- | Get the pattern that was matched in the router, e.g. @"/foo/:bar"@
routePattern :: Monad m => HandlerT m (Maybe T.Text)
routePattern = HandlerT $ view $ L.matchedPattern

lookupParam :: (Functor m, Monad m, Parsable a) => C.ByteString -> HandlerT m (Maybe a)
lookupParam k = do
  mk <- lookup k <$> params
  case mk of
    Nothing -> return Nothing
    Just v -> do
      let ev = parseParam v
      case ev of
        Left err -> do
          status badRequest400
          text $ TL.fromStrict $ decodeUtf8 err
          currentResponse >>= abort
          return Nothing
        Right r -> return $ Just r

param :: (Functor m, Monad m, Parsable a) => C.ByteString -> HandlerT m a
param k = do
  p <- lookupParam k
  case p of
    Nothing -> do
      status badRequest400
      text $ "Missing required parameter " <> TL.fromStrict (decodeUtf8 k)
      currentResponse >>= abort
      param k
    Just r -> return r

raise :: Monad m => C.ByteString -> HandlerT m ()
raise msg = do
  status badRequest400
  text $ TL.fromStrict $ decodeUtf8 msg
  currentResponse >>= abort

runHandler :: Monad m => ResponseState -> Maybe T.Text -> Request -> [Param] -> HandlerT m a -> m (Either ResponseState (a, ResponseState))
runHandler rs pat rq ps m = runEitherT $ do
  (dx, r, ()) <- RWS.runRWST (fromHandler m) (RequestState pat (qsParams ++ ps) rq) rs
  return (dx, r)
  where
    qsParams = fmap (_2 %~ fromMaybe "") (queryString rq)

liftAround :: (Monad m) => (forall a. m a -> m a) -> HandlerT m a -> HandlerT m a
liftAround f m = HandlerT $ do
  (RequestState pat ps req) <- RWS.ask
  currentState <- RWS.get
  r <- lift $ lift $ f $ runHandler currentState pat req ps m
  case r of
    Left err -> lift $ left err
    Right (dx, state') -> do
      RWS.put state'
      return dx
