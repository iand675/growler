{-# LANGUAGE OverloadedStrings #-}
module Network.EngineIO.Growler where
import           Control.Applicative ((<$>))
import           Control.Arrow (second)
import           Control.Monad.Reader
import           Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.ByteString.Builder as B
import qualified Data.HashMap.Strict as H
import           Data.Maybe (maybeToList)
import           Network.EngineIO hiding (Socket, initialize)
import qualified Network.HTTP.Types.Status as S
import           Network.Wai (queryString, requestMethod, responseLBS)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import           Web.Growler (HandlerT, bytestring, request, raw, setHeader, status, abort, currentResponse)
import           Pipes.Attoparsec (parse)
import           Pipes.Wai (producerRequestBody)

growlerAPI :: MonadIO m => ServerAPI (HandlerT m)
growlerAPI = ServerAPI
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
          Right res -> Right res

  , srvGetRequestMethod = requestMethod <$> request

  , srvRunWebSocket = \app -> do
      r <- request
      unless (WaiWS.isWebSocketsReq r) undefined
      let socketRunner src sink = liftIO $ WaiWS.runWebSockets WS.defaultConnectionOptions
            (WaiWS.getRequestHead r) app src sink
      raw socketRunner $ responseLBS S.status404 [] ""
  }
  where
    makeParams = H.fromListWith (++) . map (second maybeToList) . queryString

