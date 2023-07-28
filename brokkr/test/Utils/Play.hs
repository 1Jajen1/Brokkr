{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
module Utils.Play (
  teleportToFreeRegion
, joinGame
, FailedJoinGame(..)
, isSetupPacket
) where

import Brokkr.Packet.Encode qualified as Encode
import Utils.Client
import Data.List.NonEmpty (NonEmpty(..))
import Control.Exception
import Control.Concurrent
import qualified Data.List.NonEmpty as NonEmpty
import qualified Brokkr.Packet.ClientToServer.Play as Server
import qualified Brokkr.Packet.ServerToClient.Play as Client
import Utils.Login
import Utils.Setup
import Data.List (sort)
import Chunk.Position (pattern ChunkPos)

-- Utilities
joinGame :: Int -> Int -> TestClient -> IO ()
joinGame maxTime maxTime_large client = do
  login maxTime client
  -- first is the Login packet
  readPacket_ @(Client.PlayPacket TestDimSize) "Login (Play)" maxTime_large client $ \case
    Client.Login{} -> pure ()
    p -> throwIO $ UnexpectedSetupPacket p
  let go :: Int -> Int -> Int -> Int -> IO ()
      go 0 0 0 0 = pure ()
      go needMoreChunks needCenterChunk needDefaultSpawn needInitialPosition = do
        -- If we already have all chunks there is no need to wait for a long time as
        -- all other packets are really small and should come fast
        let maxTime' = if needMoreChunks > 0 then maxTime_large else maxTime
        readPacket_ @(Client.PlayPacket TestDimSize) "Setup" maxTime' client $ \case
          Client.SynchronizePlayerPosition{} ->
            go needMoreChunks needCenterChunk needDefaultSpawn (needInitialPosition - 1)
          Client.SetDefaultSpawnPosition{} ->
            go needMoreChunks needCenterChunk (needDefaultSpawn - 1) needInitialPosition
          Client.SetCenterChunk{} ->
            go needMoreChunks (needCenterChunk - 1) needDefaultSpawn needInitialPosition
          Client.ChunkDataAndUpdateLight{} ->
            go (needMoreChunks - 1) needCenterChunk needDefaultSpawn needInitialPosition
          p -> throwIO $ UnexpectedSetupPacket p -- TODO Ignore packets we don't care about

  let -- TODO ViewDistance and spawn from config
      viewDistance = 2
      expectedChunks = sort $ [ChunkPos x z | x <- [-viewDistance..viewDistance], z <- [-viewDistance..viewDistance]] 

  go (length expectedChunks) 1 1 1
  -- now we are ready for tests

data FailedJoinGame =
    UnexpectedSetupPacket (Client.PlayPacket TestDimSize)
  | MissingSetup
  deriving stock Show
  deriving anyclass Exception

isSetupPacket :: Client.PlayPacket n -> Bool
isSetupPacket Client.ChunkDataAndUpdateLight{} = True
isSetupPacket Client.SetDefaultSpawnPosition{} = True
isSetupPacket Client.SetCenterChunk{} = True
isSetupPacket Client.SynchronizePlayerPosition{} = True
isSetupPacket _ = False

teleportToFreeRegion :: Int -> TestClient -> IO ()
teleportToFreeRegion maxTime c@TestClient{clientTestServer = TestServer{serverFreeAreasRef}} = do
  free :| xs <- takeMVar serverFreeAreasRef
  putMVar serverFreeAreasRef $! NonEmpty.fromList xs
  sendPacket maxTime c (Encode.Packet (Encode.EstimateMin 32) $ Server.SetPlayerPosition free Client.OnGround) >>= \case
    Nothing -> pure ()
    Just e -> throwIO $ FailedToTeleport e

newtype FailedToTeleport = FailedToTeleport SomeException
  deriving stock Show
  deriving anyclass Exception

