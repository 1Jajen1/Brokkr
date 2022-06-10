module Game.Event.PlayerMoved (
  playerMoved
) where

import Game.State
import Util.Position
import Player
import qualified Data.HashSet as HS
import Chunk.Position
import qualified Network.Packet.Client.Play as C
import qualified Data.Vector as V
import Control.Monad
import Network.Connection
import qualified Network.Connection as Connection
import Optics
import Util.UUID
import Network.Packet.Client.Play.ChunkData
import Control.Monad.IO.Class
import qualified IO.Chunkloading as Chunkloading
import World.Internal
import Control.Monad.Trans.State.Strict (StateT)
import Game.Monad (GameM)
import Util.Rotation

playerMoved :: MonadIO m => Player -> Maybe Position -> GameState -> Connection.Handle -> m GameState
playerMoved p mpos st conn = do
  case mpos of
    Nothing -> updateViewPosition p Nothing >> pure st
    Just pos ->
      let p' = position .~ pos $ p
          oldPos = p ^. position
      in do
        updateViewPosition p' (Just oldPos)
        pure $ (player uid % _Just) .~ p' $ st
  where
    uid = p ^. uuid

    updateViewPosition :: MonadIO m => Player -> Maybe Position -> m ()
    updateViewPosition pl mPrevPos = do
      let newChunkPos = pl ^. position % chunkPosition
          ChunkPos x z = newChunkPos
          newChunks = HS.fromList [ChunkPos x1 z1 | x1 <- [(x - viewDistance) .. (x + viewDistance)], z1 <- [(z - viewDistance) .. (z + viewDistance)]]
          viewDistance = 2
      case mPrevPos of
        Just prevPos -> do
          let oldChunkPos = prevPos ^. chunkPosition
              ChunkPos oldX oldZ = oldChunkPos
              oldChunks = HS.fromList [ChunkPos x1 z1 | x1 <- [(oldX - viewDistance) .. (oldX + viewDistance)], z1 <- [(oldZ - viewDistance) .. (oldZ + viewDistance)]]
              toSend = HS.difference newChunks oldChunks
              toUnload = HS.difference oldChunks newChunks
          when (oldChunkPos /= newChunkPos) $ do
            liftIO $ sendPacket conn (10, C.UpdateViewPosition x z)
            sendChunks toSend
            liftIO $ sendPackets conn $ (\(ChunkPos x' z') -> (16, C.UnloadChunk x' z')) <$> V.fromList (HS.toList toUnload)
        Nothing -> do
          liftIO $ sendPacket conn (10, C.UpdateViewPosition x z)
          sendChunks newChunks
          -- TODO Handle teleport id correctly
          liftIO $ sendPacket conn (40, C.PlayerPositionAndLook (p ^. position) (p ^. rotation) (C.TeleportId 0) C.NoDismount)
      where
        sendChunks toSend = do
          let World{chunkloading} = st ^. world (p ^. dimension)
          (traverse (getAndSendChunk chunkloading) . V.fromList $ HS.toList toSend) >>= liftIO . sendPackets conn
        {-# INLINE sendChunks #-}
        -- TODO This loads in place which means during the game tick
        getAndSendChunk hdl cPos = liftIO $ Chunkloading.loadChunk hdl cPos >>= \case
          Just c -> let (!cSize, !cData) = mkChunkData c
                    in pure (cSize, C.ChunkDataAndUpdateLight cData)
          Nothing -> error ""
        {-# INLINE getAndSendChunk #-}
    {-# INLINE updateViewPosition #-}
{-# SPECIALIZE playerMoved :: Player -> Maybe Position -> GameState -> Connection.Handle -> StateT GameState (GameM IO) GameState #-}
