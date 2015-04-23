module Network.SocketIO.Growler where
import           Control.Monad.Trans (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State.Strict (StateT)
import           Network.SocketIO (initialize, RoutingTable, Socket)
import           Web.Growler (GrowlerT, HandlerT, matchAny, literal)
import           Network.EngineIO.Growler (growlerAPI)

initializeSocketIO :: MonadIO m => StateT RoutingTable (ReaderT Socket (HandlerT m)) a -> IO (HandlerT m ())
initializeSocketIO = initialize growlerAPI

socketIOHandler :: MonadIO m => HandlerT m () -> GrowlerT m ()
socketIOHandler = matchAny (literal "/socket.io")
