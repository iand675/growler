{-# LANGUAGE OverloadedStrings #-}
module Web.Growler.EventSource where
import           Blaze.ByteString.Builder (Builder)
import           Control.Monad.Trans
import           Data.Function (fix)
import           Network.HTTP.Types (hContentType, status200)
import           Network.Wai.EventSource.EventStream
import           Web.Growler

eventSource :: MonadIO m => IO ServerEvent -> HandlerT m ()
eventSource m = do
  status status200
  setHeader hContentType "text/event-stream"
  stream $ \sendChunk flush -> fix $ \loop -> do
    se <- m
    case eventToBuilder se of
      Nothing -> return ()
      Just b -> sendChunk b >> flush >> loop

