{-# LANGUAGE OverloadedStrings #-}
module RouterTests where
import Control.Lens
import Control.Monad.Trans 
import Control.Monad.State (execStateT)
import qualified Data.ByteString as B
import Data.Monoid ((<>))
import Web.Growler
import Web.Growler.Router
import Web.Growler.Types (GrowlerT(..))
import Network.HTTP.Types
import Network.Wai.Internal

testRoute = Request "GET" http11 "/api/v1/users" "" [] False undefined ["api", "v1", "users"] [] (return "") undefined ChunkedBody Nothing Nothing

pat :: GrowlerT IO ()
pat = mount (literal "/api") $ mount "/:version/users" $ get "/api" $ (text "win")

test = do
  [(_, p, _)] <- execStateT (fromGrowlerT pat) []
  return $ runRoutePattern p testRoute

main = growl id defaultConfig $ do
  get "/" $ text "Hello, World!"
  get "/:name" $ do
    name <- param "name"
    text ("Hello, " <> name <> "!")

