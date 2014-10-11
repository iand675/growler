{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Growler
(
-- ** Running a growler app
  growl
, growler
-- ** Routing
, Growler
, GrowlerT
, get
, post
, put
, delete
, patch
, matchAny
, notFound
, addRoute
, regex
, capture
, function
, literal
-- ** Handlers
, Handler
, HandlerT
, currentResponse
, abort
, status
, addHeader
, setHeader
, body
, html
, json
, file
, formData
, headers
, jsonData
, params
, raw
, redirect
, request
, stream
, text
, routePattern
-- ** Parsable
, Parsable (..)
, readEither
-- ** Internals
, BodySource (..)
, ResponseState
, RoutePattern (..)
) where
import           Control.Lens              hiding (get)
import           Control.Monad.Identity
import           Control.Monad.State       hiding (get, put)
import           Control.Monad.Trans
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe
import qualified Data.Vector               as V
import           Data.Vector.Lens
import           Network.HTTP.Types.Method
import           Network.Wai
import qualified Network.Wai.Handler.Warp  as Warp
import           Web.Growler.Handler
import           Web.Growler.Parsable
import           Web.Growler.Router
import           Web.Growler.Types         hiding (status, headers, params, request)

growl :: MonadIO m => (forall a. m a -> IO a) -> HandlerT m () -> GrowlerT m () -> IO ()
growl trans fb g = do
  app <- growler trans fb g
  putStrLn "Growling"
  Warp.run 3000 app

growler :: MonadIO m => (forall a. m a -> IO a) -> HandlerT m () -> GrowlerT m () -> IO Application
growler trans fallback (GrowlerT m) = do
  result <- trans $ execStateT m []
  return $ app (reverse result ^. vector)
  where
    app rv req respond = trans (growlerRouter rv fallback req) >>= respond

growlerRouter :: forall m. MonadIO m => V.Vector (StdMethod, RoutePattern, HandlerT m ()) -> HandlerT m () -> Request -> m Response
growlerRouter rv fb r = do
  rs <- fromMaybe (runHandler initialState Nothing r [] fb) $ join $ V.find isJust $ V.map processResponse rv
  let (ResponseState status' groupedHeaders body') = either id snd rs
  let headers = concatMap (\(k, vs) -> map (\v -> (k, v)) vs) $ HM.toList groupedHeaders
  return $! case body' of
    FileSource (fpath, fpart) -> responseFile    status' headers fpath fpart
    BuilderSource b           -> responseBuilder status' headers b
    LBSSource lbs             -> responseLBS     status' headers lbs
    StreamSource sb           -> responseStream  status' headers sb
    RawSource f r'            -> responseRaw     f      r'
  where
    processResponse (m, pat, respond) = case route r m pat of
      Nothing -> Nothing
      Just ps -> Just $ runHandler initialState (Just pat) r ps respond

