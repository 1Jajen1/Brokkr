module Event.Handler.PlayerDisconnected (
  playerDisconnected
) where

import Game.State
import Player
import Control.Monad.State.Class
import qualified Network.Connection.Internal as Connection
import Control.Monad.Trans.State.Strict (StateT)
import Game.Monad (GameM)
import Control.Monad.IO.Class
import Optics
import Util.UUID
import Util.Disconnect
import World
import World.Internal
import Chunk.Position
import Util.Position
import Data.Foldable (foldl')

playerDisconnected :: (MonadIO m, MonadState GameState m) => Player -> DisconnectReason -> m ()
playerDisconnected p reason = do
  st <- get
  let Just conn = st ^. connection uid
  liftIO $ case reason of
    ClientDisconnect -> Connection.close conn
    ServerDisconnect r -> Connection.disconnect conn r

  let !newChunkPos = p ^. position % chunkPosition
      !(ChunkPos x z) = newChunkPos
      !chunks = [ChunkPos x1 z1 | x1 <- [(x - viewDistance) .. (x + viewDistance)], z1 <- [(z - viewDistance) .. (z + viewDistance)]]
      !viewDistance = 8

  put $!
    st & connection uid .~ Nothing
       & player uid .~ Nothing
       & world dim %~ (\w -> foldl' unloadChunk w chunks)
  -- TODO Unload the players chunks
  
  where
    uid = p ^. uuid
    dim = p ^. dimension
{-# SPECIALIZE playerDisconnected :: Player -> DisconnectReason -> StateT GameState (GameM IO) () #-}
