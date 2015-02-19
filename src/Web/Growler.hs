{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|

A Haskell web framework inspired by the Scotty framework, with an
eye towards performance, extensibility, and ease of use.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
> import Data.Monoid ((<>))
> import Web.Growler
>
> main = growl id defaultConfig $ do
>   get "/" $ text "Hello, World!"
>   get "/:name" $ do
>     name <- param "name"
>     text ("Hello, " <> name <> "!")

-}
module Web.Growler
(
-- ** Running a growler app
  growl
, growler
, defaultConfig
, GrowlerConfig (..)
-- ** Routing
, Growler
, GrowlerT
, regex
, capture
, function
, literal
, mount
, handlerHook
, notFound
-- *** HTTP Methods
, get
, post
, put
, delete
, patch
, matchAny
-- *** Primitives
, addRoute
-- ** Handlers
, Handler
, HandlerT
-- *** Primitive request functions
, request
, routePattern
, params
-- *** Primitive response functions
, file
, builder
, bytestring
, stream
, raw
, currentResponse
, abort
-- *** Convenience functions
-- **** Request helpers
, lookupParam
, param
, formData
, headers
, jsonData
-- **** Response helpers
, status
, addHeader
, setHeader
, raise
, redirect
, text
, html
, json
, JsonInputError (..)
, DecodingError (..)
-- ** Parsable
, Parsable (..)
, readEither
-- ** Internals
, body
, BodySource (..)
, ResponseState (..)
, RoutePattern (..)
) where
import           Control.Exception                (catch)
import           Control.Lens                     hiding (get)
import           Control.Monad.Identity
import           Control.Monad.Trans.State.Strict hiding (get, put)
import           Control.Monad.Trans
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe
import qualified Data.Vector                      as V
import           Data.Vector.Lens
import           Network.HTTP.Types.Method
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp
import           Pipes.Aeson                      (DecodingError (..))
import           Web.Growler.Handler
import           Web.Growler.Parsable
import           Web.Growler.Router
import           Web.Growler.Types                hiding (status, headers, params, request, capture)

-- | The simple approach to starting up a web server
growl :: MonadIO m => (forall a. m a -> IO a) -- ^ A function to convert your base monad of choice into IO.
                   -> GrowlerConfig m
                   -> GrowlerT m ()           -- ^ The router for all the other routes
                   -> IO ()
growl trans fb g = do
  app <- growler trans fb g
  putStrLn "Growling"
  Warp.run 3000 app

-- | For more complex needs, access to the actual WAI 'Application'. Useful for adding middleware.
growler :: MonadIO m => (forall a. m a -> IO a) -- ^ A function to convert your base monad of choice into IO.
                     -> GrowlerConfig m
                     -> GrowlerT m ()           -- ^ The router for all the other routes
                     -> IO Application
growler trans (GrowlerConfig nf er) (GrowlerT m) = do
  result <- trans $ execStateT m []
  return $ app (reverse result ^. vector)
  where
    app rv req respond = catch (trans (growlerRouter rv nf req) >>= respond) $ \e -> do
      mr <- trans (runHandler initialState Nothing req [] (er e))
      let (ResponseState status' groupedHeaders body') = either id snd mr
      let headers = concatMap (\(k, vs) -> map (\v -> (k, v)) vs) $ HM.toList groupedHeaders
      respond $ case body' of
        FileSource fpath fpart    -> responseFile    status' headers fpath fpart
        BuilderSource b           -> responseBuilder status' headers b
        LBSSource lbs             -> responseLBS     status' headers lbs
        StreamSource sb           -> responseStream  status' headers sb
        RawSource f r'            -> responseRaw     f       r'

growlerRouter :: forall m. MonadIO m => V.Vector (StdMethod, RoutePattern, HandlerT m ()) -> HandlerT m () -> Request -> m Response
growlerRouter rv fb r = do
  rs <- fromMaybe (runHandler initialState Nothing r [] fb) $ join $ V.find isJust $ V.map processResponse rv
  let (ResponseState status' groupedHeaders body') = either id snd rs
  let headers = concatMap (\(k, vs) -> map (\v -> (k, v)) vs) $ HM.toList groupedHeaders
  return $! case body' of
    FileSource fpath fpart    -> responseFile    status' headers fpath fpart
    BuilderSource b           -> responseBuilder status' headers b
    LBSSource lbs             -> responseLBS     status' headers lbs
    StreamSource sb           -> responseStream  status' headers sb
    RawSource f r'            -> responseRaw     f       r'
  where
    processResponse (m, pat, respond) = case route r m pat of
      Nothing -> Nothing
      Just (patRep, ps) -> Just $ runHandler initialState (Just patRep) r ps respond

defaultConfig :: MonadIO m => GrowlerConfig m
defaultConfig = GrowlerConfig notFound $ \e -> do
  liftIO $ print e
  internalServerError
