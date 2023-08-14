{-# LANGUAGE UndecidableInstances #-}
module Brokkr.System.PlayerMovement (
  playerMovement
, OldPosition(..)
, OldRotation(..)
, Land, Fly
, ChunkYPosition(..)
) where

import Brokkr.Chunk.Internal (force)
import Brokkr.Chunk.Position
import Brokkr.Client

import Brokkr.Debug.Monad
import Brokkr.Dimension

import Brokkr.IO.ChunkCache (ChunkTicket(ChunkTicket))

import Brokkr.Network.Connection
import Brokkr.Network.Util.Chunk (mkChunkPacket)

import Brokkr.Packet.ServerToClient.Play qualified as Packet

import Brokkr.Server.Config
import Brokkr.Server.Monad (Server)
import Brokkr.Server.Monad qualified as Server

import Brokkr.Util.Linear.Vector
import Brokkr.Util.Position
import Brokkr.Util.Rotation

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM

import Data.Coerce
import Data.Proxy

import Control.Monad
import Control.Monad.IO.Class

import Foreign.Storable

newtype OldPosition = OldPosition Position
  deriving stock Show
  deriving newtype (Eq, Storable)
  deriving Server.Component via (Server.ViaFlat OldPosition)

newtype OldRotation = OldRotation Rotation
  deriving stock Show
  deriving newtype (Eq, Storable)
  deriving Server.Component via (Server.ViaFlat OldRotation)

newtype OldFalling = OldFalling Falling
  deriving stock Show
  deriving newtype Eq
  deriving Server.Component via (Server.ViaBox Falling)

data Land
data Fly

newtype ChunkYPosition = ChunkYPosition Int
  deriving stock Show
  deriving newtype (Eq, Storable)
  deriving Server.Component via (Server.ViaFlat Int)

-- TODO When writing out position changes merge them into their respective combined packets
-- ^-- No? Instead query for enabled and disabled and check when iterating?
-- TODO Make sure Old-* components are only enabled and disabled. This avoids table moves for
--  movement changes. In general table moves should be avoided

-- | Run systems for managing player movement.
-- Handles chunk loading and unloading, fall damage and sending
-- the updated player position to clients. 
playerMovement :: Server ()
playerMovement = do
  -- Position changes
  Server.system (Server.filterDSL @'[Connection, Position, OldPosition, ChunkYPosition]) $ \aty -> do
    connRef <- Server.getColumn @Connection aty

    posRef <- Server.getColumn @Position aty
    oldPosRef <- Server.getColumn @OldPosition aty
    chunkYRef <- Server.getColumn @ChunkYPosition aty

    -- rotRef <- Server.getColumn @Rotation aty
    
    Server.iterateArchetype_ aty $ \n eid -> do
      conn <- Server.readColumn connRef n

      position <- Server.readColumn posRef n
      let Position _ y _ = position
      oldPosition <- Server.readColumn oldPosRef n
      oldChunkY <- Server.readColumn chunkYRef n

      -- _rotation <- Server.readColumn rotRef n

      -- TODO Disable instead
      Server.remove @OldPosition eid
      -- Server.remove @OldRotation eid

      -- Update the view position and maybe load chunks
      let oldChunkPos@(ChunkPosition oldChunkX oldChunkZ) = toChunkPos $ coerce oldPosition
          newChunkPos@(ChunkPosition chunkX chunkZ) = toChunkPos position
          (ChunkPosition deltaChunkX deltaChunkZ) = toDelta oldChunkPos newChunkPos
          acrossBorder = 0 /= deltaChunkX || 0 /= deltaChunkZ
          chunkY = floor y `div` 16

      Server.writeColumn chunkYRef n $ ChunkYPosition chunkY
      -- liftIO $ print (chunkY, oldChunkY)

      -- Yes, I too have no idea why we need to send SetCenterChunk or why the y level change wants requires it
      when (acrossBorder || chunkY /= coerce oldChunkY) $ do
        -- liftIO . putStrLn $ "Across " <> show (acrossBorder, oldChunkY, chunkY)
        -- send SetCenterChunk packet
        liftIO . sendPacket @UnsafeDefDimHeight conn $! Packet (EstimateMin 32) (Packet.SetCenterChunk (fromIntegral chunkX) (fromIntegral chunkZ))

        when acrossBorder $ do
          viewDistance <- configServerRenderDistance <$> Server.getConfig
          -- TODO optimise all of this at some point. Also doesn't minecraft do circles now?
          --  Just use one pattern and add the players center to that to shift it instead of recalculating it
          let oneRow      = [-viewDistance..viewDistance]
              loadXRows   = [ChunkPosition (oldChunkX + signum deltaChunkX * (dX + viewDistance)) (chunkZ + z) | z <- oneRow, dX <- [1..(abs deltaChunkX)]]
              loadZRows   = [ChunkPosition (chunkX + x) (oldChunkZ + signum deltaChunkZ * (dZ + viewDistance)) | x <- oneRow, dZ <- [1..(abs deltaChunkZ)]]
              unloadXRows = [ChunkPosition (chunkX + negate (signum deltaChunkX) * (dX + viewDistance)) (oldChunkZ + z) | z <- oneRow, dX <- [1..(abs deltaChunkX)]]
              unloadZRows = [ChunkPosition (oldChunkX + x) (chunkZ + negate (signum deltaChunkZ) * (dZ + viewDistance)) | x <- oneRow, dZ <- [1..(abs deltaChunkZ)]]
              playerTicket = ChunkTicket eid

          -- This is sort of static, so lookup at the start?
          dim <- Server.get @Dimension eid pure $ error "Client without dimension" -- TODO

          -- debug @Verbose   $ "Requesting " <> show (length $ loadXRows <> loadZRows) <> " chunks"
          -- debug @Verbose $ show $ loadXRows <> loadZRows

          withChunkLoading dim $ \(Proxy :: Proxy dimHeight) loadChunk -> liftIO $ forM_ (loadXRows <> loadZRows) $ \cpos -> do
            as <- Async.async $ loadChunk playerTicket cpos >>= \ref -> do
              !c <- atomically $ do
                !c <- force <$> readTVar ref
                writeTVar ref c
                pure c
              let !(!sz, !cD) = mkChunkPacket c
              sendPacket @dimHeight conn $! Packet (EstimateMin sz) $! cD
            Async.link as

          liftIO . forM_ (unloadXRows <> unloadZRows) $ \(ChunkPosition x z) -> do
            -- TODO Remove playerTicket from the chunk cache
            sendPacket @UnsafeDefDimHeight conn $! Packet (EstimateMin 32) $ Packet.UnloadChunk (fromIntegral x) (fromIntegral z)

      -- TODO Append the move to be sent to other clients
      -- pure ()
  where
    toDelta (ChunkPosition lX lZ) (ChunkPosition rX rZ) = ChunkPosition (rX - lX) (rZ - lZ)
    toChunkPos (Position x _ z) = ChunkPosition (floor x `div` 16) (floor z `div` 16)
