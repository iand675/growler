{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Growler.Router
    ( get
    , post
    , put
    , delete
    , patch
    , addRoute
    , matchAny
    , capture
    , regex
    , function
    , mount
    , literal
    , route
    , handlerHook
    , RoutePattern(..)
    , notFound
    , internalServerError
    ) where

import           Control.Arrow                    ((***))

import           Control.Monad.Trans
import qualified Control.Monad.Trans.State.Strict as S

import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy.Char8       as BL
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      ((<>), mconcat)
import           Data.String                      (fromString)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Lazy                   as TL

import           Network.HTTP.Types
import           Network.Wai                      (Request (..))
import qualified Network.Wai.Parse                as Parse

import qualified Text.Regex                       as Regex

import           Web.Growler.Handler
import           Web.Growler.Types                hiding (status, capture)

-- TODO shim to add support for transformers 0.3. Get rid of it once
-- Nix makes it easier to escape older transformers version.
modify' f = S.get >>= (($!) S.put . f)

mount :: Monad m => RoutePattern -> GrowlerT m () -> GrowlerT m ()
mount pat m = GrowlerT $ do
  previous <- S.get
  -- create inner scope that doesn't affect external routes
  S.put []
  fromGrowlerT m
  modify' (fmap $ \(m, p, h) -> (m, pat <> p, h))
  new <- S.get
  S.put (new ++ previous)


handlerHook :: Monad m => (HandlerT m () -> HandlerT m ()) -> GrowlerT m ()
handlerHook f = GrowlerT $ modify' (fmap $ \(m, p, h) -> (m, p, f h))

-- | get = 'addRoute' 'GET'
get :: (MonadIO m) => RoutePattern -> HandlerT m () -> GrowlerT m ()
get = addRoute GET

-- | post = 'addRoute' 'POST'
post :: (MonadIO m) => RoutePattern -> HandlerT m () -> GrowlerT m ()
post = addRoute POST

-- | put = 'addRoute' 'PUT'
put :: (MonadIO m) => RoutePattern -> HandlerT m () -> GrowlerT m ()
put = addRoute PUT

-- | delete = 'addRoute' 'DELETE'
delete :: (MonadIO m) => RoutePattern -> HandlerT m () -> GrowlerT m ()
delete = addRoute DELETE

-- | patch = 'addRoute' 'PATCH'
patch :: (MonadIO m) => RoutePattern -> HandlerT m () -> GrowlerT m ()
patch = addRoute PATCH

-- | Add a route that matches regardless of the HTTP verb.
matchAny :: (MonadIO m) => RoutePattern -> HandlerT m () -> GrowlerT m ()
matchAny pattern action = mapM_ (\v -> addRoute v pattern action) [minBound..maxBound]

-- | A blank 404 Not Found handler for convenience.
notFound :: (Monad m) => HandlerT m ()
notFound = status status404

-- | A blank 500 Internal Server Error handler for convenience.
internalServerError :: Monad m => HandlerT m ()
internalServerError = status status500

-- | Define a route with a 'StdMethod', 'T.Text' value representing the path spec,
-- and a body ('Action') which modifies the response.
--
-- > addroute GET "/" $ text "beam me up!"
--
-- The path spec can include values starting with a colon, which are interpreted
-- as /captures/. These are named wildcards that can be looked up with 'param'.
--
-- > addroute GET "/foo/:bar" $ do
-- >     v <- param "bar"
-- >     text v
--
-- >>> curl http://localhost:3000/foo/something
-- something
addRoute :: (MonadIO m) => StdMethod -> RoutePattern -> HandlerT m () -> GrowlerT m ()
addRoute method pat action = GrowlerT $ modify' ((method, pat, action):)

route :: Request -> StdMethod -> RoutePattern -> Maybe (T.Text, [Param])
route req method pat = if Right method == parseMethod (requestMethod req)
  then matchRoute pat req
  else Nothing

matchRoute :: RoutePattern -> Request -> Maybe (T.Text, [Param])
matchRoute (RoutePattern p) req = let (RoutePatternResult pat _ rps) = p req in case rps of
  Fail -> Nothing
  Partial _ -> Nothing
  Complete ps -> Just (pat, ps)

-- Pretend we are at the top level.
parseEncodedParams :: B.ByteString -> [Param]
parseEncodedParams bs = [ (T.encodeUtf8 k, T.encodeUtf8 $ fromMaybe "" v) | (k,v) <- parseQueryText bs ]

-- | Match requests using a regular expression.
--   Named captures are not yet supported.
--
-- > get (regex "^/f(.*)r$") $ do
-- >    path <- param "0"
-- >    cap <- param "1"
-- >    text $ mconcat ["Path: ", path, "\nCapture: ", cap]
--
-- >>> curl http://localhost:3000/foo/bar
-- Path: /foo/bar
-- Capture: oo/ba
--
regex :: String -> RoutePattern
regex pattern = RoutePattern go
  where
    go req = RoutePatternResult (T.pack pattern) req $ maybe Fail Complete $ fmap convertParams match
      where
        rgx = Regex.mkRegex pattern
        strip (_, match, _, subs) = match : subs
        match = Regex.matchRegexAll rgx $ T.unpack $ path req
        convertParams = map (B.pack . show *** (T.encodeUtf8 . T.pack)) . zip [0 :: Int ..] . strip

-- | Standard Sinatra-style route. Named captures are prepended with colons.
--   This is the default route type generated by OverloadedString routes. i.e.
--
-- > get (capture "/foo/:bar") $ ...
--
--   and
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > ...
-- > get "/foo/:bar" $ ...
--
--   are equivalent.
capture :: String -> RoutePattern
capture = fromString

-- | Build a route based on a function which can match using the entire 'Request' object.
--   'Nothing' indicates the route does not match. A 'Just' value indicates
--   a successful match, optionally returning a list of key-value pairs accessible
--   by 'param'.
--
-- > get (function $ \req -> Just [("version", T.pack $ show $ httpVersion req)]) $ do
-- >     v <- param "version"
-- >     text v
--
-- >>> curl http://localhost:3000/
-- HTTP/1.1
--
function :: (Request -> T.Text) -> (Request -> MatchResult) -> RoutePattern
function fn fps = RoutePattern $ \r -> RoutePatternResult (fn r) r (fps r)

-- | Build a route that requires the requested path match exactly, without captures.
literal :: String -> RoutePattern
literal pat = RoutePattern go
  where
    go req = RoutePatternResult packed (req { pathInfo = req' }) result
      where
        packed = T.pack pat
        (result, req') = case T.stripPrefix packed (path req) of
          Nothing -> (Fail, [])
          Just rem -> if T.null rem then (Complete [], []) else (Partial [], dropWhile (== "") $ T.split (== '/') rem)
