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
    , notFound
    , capture
    , regex
    , function
    , literal
    , route
    , RoutePattern(..)
    ) where

import           Control.Arrow              ((***))

import           Control.Monad.State        hiding (get, put)
import           Control.Monad.Trans

import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (mconcat)
import           Data.String                (fromString)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as TL

import           Network.HTTP.Types
import           Network.Wai                (Request (..))
import qualified Network.Wai.Parse          as Parse

import qualified Text.Regex                 as Regex

import           Web.Growler.Handler
import           Web.Growler.Types          hiding (status)

-- | get = 'addroute' 'GET'
get :: (MonadIO m) => RoutePattern -> HandlerT m () -> GrowlerT m ()
get = addRoute GET

-- | post = 'addroute' 'POST'
post :: (MonadIO m) => RoutePattern -> HandlerT m () -> GrowlerT m ()
post = addRoute POST

-- | put = 'addroute' 'PUT'
put :: (MonadIO m) => RoutePattern -> HandlerT m () -> GrowlerT m ()
put = addRoute PUT

-- | delete = 'addroute' 'DELETE'
delete :: (MonadIO m) => RoutePattern -> HandlerT m () -> GrowlerT m ()
delete = addRoute DELETE

-- | patch = 'addroute' 'PATCH'
patch :: (MonadIO m) => RoutePattern -> HandlerT m () -> GrowlerT m ()
patch = addRoute PATCH

-- | Add a route that matches regardless of the HTTP verb.
matchAny :: (MonadIO m) => RoutePattern -> HandlerT m () -> GrowlerT m ()
matchAny pattern action = mapM_ (\v -> addRoute v pattern action) [minBound..maxBound]

-- | Specify an action to take if nothing else is found. Note: this _always_ matches,
-- so should generally be the last route specified.
notFound :: (MonadIO m) => HandlerT m ()
notFound = status status404

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
addRoute method pat action = GrowlerT $ modify ((method, pat, action):)

route :: Request -> StdMethod -> RoutePattern -> Maybe [Param]
route req method pat = if Right method == parseMethod (requestMethod req)
  then matchRoute pat req
  else Nothing

matchRoute :: RoutePattern -> Request -> Maybe [Param]
matchRoute (Literal pat)  req | pat == path req = Just []
                              | otherwise       = Nothing
matchRoute (Function _ fun) req = fun req
matchRoute (Capture pat)  req = go (T.split (== '/') pat) (T.split (== '/') $ path req) []
    where go [] [] prs = Just prs -- request string and pattern match!
          go [] r  prs | T.null (mconcat r)  = Just prs -- in case request has trailing slashes
                       | otherwise           = Nothing  -- request string is longer than pattern
          go p  [] prs | T.null (mconcat p)  = Just prs -- in case pattern has trailing slashes
                       | otherwise           = Nothing  -- request string is not long enough
          go (p:ps) (r:rs) prs | p == r          = go ps rs prs -- equal literals, keeping checking
                               | T.null p        = Nothing      -- p is null, but r is not, fail
                               | T.head p == ':' = go ps rs $ (T.encodeUtf8 $ T.tail p, T.encodeUtf8 r) : prs -- p is a capture, add to params
                               | otherwise       = Nothing      -- both literals, but unequal, fail

-- Pretend we are at the top level.
path :: Request -> T.Text
path = T.cons '/' . T.intercalate "/" . pathInfo

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
regex pattern = Function (const $ T.pack pattern) $ \ req -> fmap (map (B.pack . show *** (T.encodeUtf8 . T.pack)) . zip [0 :: Int ..] . strip)
                                         (Regex.matchRegexAll rgx $ T.unpack $ path req)
    where rgx = Regex.mkRegex pattern
          strip (_, match, _, subs) = match : subs

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
function :: (Request -> T.Text) -> (Request -> Maybe [Param]) -> RoutePattern
function = Function

-- | Build a route that requires the requested path match exactly, without captures.
literal :: String -> RoutePattern
literal = Literal . T.pack
