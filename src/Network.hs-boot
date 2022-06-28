module Network (
  runProtocol
) where
import Control.Monad.IO.Class (MonadIO)
import Network.Monad (MonadNetwork)
import Network.Protocol (Protocol)
import qualified Network.Connection as Connection
import Util.Log (MonadLog)
import Game.State (MonadGameState)
import Data.UUID
import Util.Exception (MonadBracket)

runProtocol ::
  ( MonadIO m
  , MonadBracket m
  , MonadNetwork m
  , MonadLog m
  , MonadGameState m
  ) => (UUID -> Protocol -> IO Connection.Handle) -> m ()
