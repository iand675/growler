{-# LANGUAGE OverloadedStrings #-}
module Web.Growler.EngineIO where
import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad (unless)
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.ByteString.Builder as B
import qualified Data.HashMap.Strict as H
import           Data.Maybe (maybeToList)
import           Network.EngineIO
import qualified Network.HTTP.Types.Status as S
import           Network.Wai (Response, queryString, requestMethod)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import           Web.Growler (HandlerT, bytestring, request, raw, setHeader, status, abort, currentResponse)
import           Pipes
import           Pipes.Attoparsec (parse)
import           Pipes.Wai (producerRequestBody)

growlerAPI :: MonadIO m => Response -> ServerAPI (HandlerT m)
growlerAPI response = ServerAPI
  { srvTerminateWithResponse = \code ct b -> do
      let aStatus = filter ((==) code . S.statusCode) [S.status100..S.status511]
      case aStatus of
        [] -> error "not a valid status code"
        (st:_) -> do
          status st
          setHeader "Content-Type" ct
          bytestring $ B.toLazyByteString b
          currentResponse >>= abort
          undefined
  
  , srvGetQueryParams = makeParams <$> request
  
  , srvParseRequestBody = \p -> do
      r <- request
      eps <- evalStateT (parse p) $ producerRequestBody r
      return $ case eps of
        Nothing -> Left "Exhausted request body"
        Just ps -> case ps of
          Left err -> Left $ show err
          Right r -> Right r
  
  , srvGetRequestMethod = requestMethod <$> request
  
  , srvRunWebSocket = \app -> do
      r <- request
      unless (WaiWS.isWebSocketsReq r) undefined
      let socketRunner src sink = liftIO $ WaiWS.runWebSockets WS.defaultConnectionOptions
            (WaiWS.getRequestHead r) app src sink
      raw socketRunner response
  }
  where
    makeParams = H.fromListWith (++) . map (second maybeToList) . queryString
