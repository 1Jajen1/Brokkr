{-# LANGUAGE DeriveAnyClass #-}
module Utils.Play (
  teleportToFreeRegion
, joinGame
, FailedJoinGame(..)
, isSetupPacket
) where

import Utils.Client
import Data.List.NonEmpty (NonEmpty(..))
import Control.Exception
import Control.Concurrent
import qualified Data.List.NonEmpty as NonEmpty
import qualified Network.Packet.Server.Play as Server
import qualified Network.Packet.Client.Play as Client
import Util.Position
import Utils.Login
import Utils.Setup
import Control.Monad.Fix

-- Utilities
joinGame :: Int -> TestClient -> IO ()
joinGame maxTime client = do
  login maxTime client
  -- first is the Login packet
  readPacket_ "Login (Play)" maxTime client $ \case
    Client.Login _ -> pure ()
    p -> throwIO $ UnexpectedSetupPacket p
  -- next consume all the setup packets that were sent
  fix $ \go -> do
    -- todo catch the timeout and abort the loop
    readPacketOrTimeout_ "Setup" maxTime client $ \case
      Nothing -> pure ()
      Just p ->
        if isSetupPacket p then go
        else throwIO $ UnexpectedSetupPacket p
    go
  -- now we are ready for tests

newtype FailedJoinGame = UnexpectedSetupPacket Client.Packet
  deriving stock Show
  deriving anyclass Exception

isSetupPacket :: Client.Packet -> Bool
isSetupPacket Client.ChunkDataAndUpdateLight{} = True
isSetupPacket Client.SetDefaultSpawnPosition{} = True
isSetupPacket Client.SetCenterChunk{} = True
isSetupPacket Client.SynchronizePlayerPosition{} = True
isSetupPacket _ = False

teleportToFreeRegion :: Int -> TestClient -> IO ()
teleportToFreeRegion maxTime c@TestClient{clientTestServer = TestServer{serverFreeAreasRef}} = do
  free :| xs <- takeMVar serverFreeAreasRef
  putMVar serverFreeAreasRef $! NonEmpty.fromList xs
  sendPacket maxTime c 32 (Server.SetPlayerPosition free OnGround) >>= \case
    Nothing -> pure ()
    Just e -> throwIO $ FailedToTeleport e

newtype FailedToTeleport = FailedToTeleport SomeException
  deriving stock Show
  deriving anyclass Exception

