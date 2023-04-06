{-# LANGUAGE UndecidableInstances #-}
module System.PlayerMovement (
  playerMovement
, Translate(..)
, Rotate(..)
, Land, Fly
) where

import Chunk.Position
import Client
import Control.Monad
import Control.Monad.IO.Class

import Control.Concurrent.MVar
import qualified Control.Concurrent.Async as Async

import Data.Coerce
import Dimension

import Network.Connection
import qualified Network.Packet.Client.Play as C
import qualified Network.Packet.Client.Play.ChunkData as C

import Foreign.Storable

import IO.Chunkloading

import Util.Linear.Vector
import Util.Linear.V2
import Util.Linear.V3
import Util.Position
import Util.Rotation

import Server.Config
import Server.Monad (Server)
import Server.Monad qualified as Server

newtype Translate = Translate (V3 Double)
  deriving stock Show
  deriving newtype (Eq, Storable)
  deriving Server.Component via (Server.ViaFlat Translate)

newtype Rotate = Rotate (V2 Float)
  deriving stock Show
  deriving newtype (Eq, Storable)
  deriving Server.Component via (Server.ViaFlat Rotate)

data Land
data Fly

playerMovement :: Server ()
playerMovement = Server.system
  (Server.filterDSL @'[Server.Tag Joined, Connection, Position, Rotation, Translate, Rotate])
  $ \aty -> do
    connRef <- Server.getColumn @Connection aty
    posRef  <- Server.getColumn @Position aty
    rotRef  <- Server.getColumn @Rotation aty
    translateRef <- Server.getColumn @Translate aty
    rotateRef    <- Server.getColumn @Rotate aty

    Server.iterateArchetype aty $ \n eid -> do
      conn <- Server.readColumn connRef n

      position <- Server.readColumn posRef n
      rotation <- Server.readColumn rotRef n

      translate@(Translate (V3_Double _ dY _ )) <- Server.readColumn translateRef n
      rotate <- Server.readColumn rotateRef n

      -- TODO Check if Translate is enabled (for now just set to 0)
      let newPosition = position |+| coerce translate
      Server.writeColumn posRef n newPosition
      Server.writeColumn translateRef n $ Translate (V3_Double 0 0 0)

      -- TODO Check if Rotate is enabled
      Server.writeColumn rotRef n $ rotation |+| coerce rotate
      Server.writeColumn rotateRef n $ Rotate (V2_Float 0 0)

      -- Update the view position and maybe load chunks
      let oldChunkPos@(ChunkPos oldChunkX oldChunkZ) = toChunkPos position
          newChunkPos@(ChunkPos chunkX chunkZ) = toChunkPos newPosition
          delta@(ChunkPos deltaChunkX deltaChunkZ) = toDelta oldChunkPos newChunkPos
          acrossBorder = delta /= ChunkPos 0 0

      -- Yes, I too have no idea why we need to send SetCenterChunk or why the y level change wants requires it
      when (acrossBorder || abs dY >= 1) $ do
        -- send SetCenterChunk packet
        liftIO . sendPacket conn $! SendPacket 32 (C.SetCenterChunk chunkX chunkZ)

        when acrossBorder $ do
          viewDistance <- configServerRenderDistance <$> Server.getConfig
              -- TODO optimise all of this at some point. Also doesn't minecraft do circles now?
          let oneRow      = [-viewDistance..viewDistance]
              loadXRows   = [ChunkPos (oldChunkX + signum deltaChunkX * (dX + viewDistance)) (chunkZ + z) | z <- oneRow, dX <- [1..(abs deltaChunkX)]]
              loadZRows   = [ChunkPos (chunkX + x) (oldChunkZ + signum deltaChunkZ * (dZ + viewDistance)) | x <- oneRow, dZ <- [1..(abs deltaChunkZ)]]
              unloadXRows = [ChunkPos (chunkX + negate (signum deltaChunkX) * (dX + viewDistance)) (oldChunkZ + z) | z <- oneRow, dX <- [1..(abs deltaChunkX)]]
              unloadZRows = [ChunkPos (oldChunkX + x) (chunkZ + negate (signum deltaChunkZ) * (dZ + viewDistance)) | x <- oneRow, dZ <- [1..(abs deltaChunkZ)]]
          
          chunkloading <- Server.getSingleton @Chunkloading
          -- This is sort of static, so lookup at the start?
          dim <- Server.get @Dimension eid pure $ error "Client without dimension" -- TODO
          rPath <- Server.get @RegionFilePath (Dimension.entityId dim) pure $ error "Dimension without a regionfile" -- TODO
          
          liftIO $ loadChunks (coerce rPath) (loadXRows <> loadZRows) chunkloading $ \mvar -> void . Async.async $ takeMVar mvar >>= \c -> do
            let !(!sz, !cData) = C.mkChunkData c
            sendPacket conn . SendPacket sz $! C.ChunkDataAndUpdateLight cData
          
          liftIO . forM_ (unloadXRows <> unloadZRows) $ \(ChunkPos x z) -> sendPacket conn $! SendPacket 32 $ C.UnloadChunk x z
  where
    toDelta (ChunkPos lX lZ) (ChunkPos rX rZ) = ChunkPos (rX - lX) (rZ - lZ)
    toChunkPos (Position x _ z) = ChunkPos (floor x `div` 16) (floor z `div` 16)

-- TODO Handle OnGround changes
