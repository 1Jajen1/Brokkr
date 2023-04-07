module JoinGameSpec (spec) where

import Test.Syd

import Utils.Setup
import Utils.Login
import Utils.Play
import qualified Network.Packet.Client.Play as Client
import qualified Network.Packet.Client.Play.ChunkData as Client
import Control.Exception
import Data.Maybe (mapMaybe)
import Data.List (sort)
import Chunk.Position
import Block.Position
import Util.Position
import Util.Rotation

spec :: BrokkrSpec
spec = do
  -- TODO sequential doesn't seem to work?
  sequential $ withTestClient "JoinGame1" $ do
    it "should receive all the required setup packets" $ \client -> do
      -- These values are guesswork and make the test flaky...
      -- The chunk values work for cold loads and should get much much
      -- better once I cache chunks, so this is probably fine for a while
      let maxTime = 1_000
          -- max time for some of the larger packets
          maxTime_large = 10_000

      login maxTime client

      readPacket_ "Login (play)" maxTime_large client $ \case
          Client.Login _ -> pure ()
          p -> unexpectedPacket "Login (Play)" p

      -- read packets until the server stops sending
      -- Accepts all packets that are categorized as setup
      -- This *only* works with sequential for the test
      let go needMoreChunks buf = do
            -- If we already have all chunks there is no need to wait for a long time as
            -- all other packets are really small and should come fast
            let maxTime' = if needMoreChunks > 0 then maxTime_large else maxTime
            readPacketOrTimeout_ "Login (play)" maxTime' client $ \case
              Nothing -> pure buf
              Just p@Client.ChunkDataAndUpdateLight{} -> go (needMoreChunks - 1) (p:buf)
              Just p -> if isSetupPacket p then go needMoreChunks (p:buf) else throwIO $ UnexpectedSetupPacket p

      let -- TODO ViewDistance and spawn from config
          viewDistance = 2
          expectedChunks = sort $ [ChunkPos x z | x <- [-viewDistance..viewDistance], z <- [-viewDistance..viewDistance]] 

      res <- go (length expectedChunks) []

      -- check that all chunks arrived
      let receivedChunks = sort $ mapMaybe (\case
            Client.ChunkDataAndUpdateLight (Client.ChunkData _ pos _ _ _ _ _ _) -> Just pos
            _ -> Nothing
            ) res
      receivedChunks `shouldBe` expectedChunks

      -- check that the default spawn has been set
      let receivedDefaultSpawn = filter (\case
            Client.SetDefaultSpawnPosition{} -> True
            _ -> False
            ) res
          -- TODO Get values from config
          expectedDefaultSpawn = [Client.SetDefaultSpawnPosition (BlockPos 0 130 0) 0]
      receivedDefaultSpawn `shouldBe` expectedDefaultSpawn

      -- check that the center check has been set
      let receivedCenterChunk = filter (\case
            Client.SetCenterChunk{} -> True
            _ -> False
            ) res
          -- TODO Get values from config
          expectedCenterChunk = [Client.SetCenterChunk 0 0]
      receivedCenterChunk `shouldBe` expectedCenterChunk

      -- check that we have received an initial position
      let receivedInitialPosition = filter (\case
            Client.SynchronizePlayerPosition{} -> True
            _ -> False
            ) res
          -- TODO Get values from config
          expectedInitialPosition = [Client.SynchronizePlayerPosition (Position 0 130 0) (Rotation 180 0) (Client.TeleportId 0) Client.NoDismount]
      receivedInitialPosition `shouldBe` expectedInitialPosition

      pure ()

  describe "disconnect after login" $ do
    withTestClient "JoinGame2" $ do
      it "should not crash the server" $ \client -> do
        let maxTime = 1_000
        login maxTime client
        -- TODO check that the server did the cleanup properly once I can

