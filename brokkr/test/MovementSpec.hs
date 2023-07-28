module MovementSpec (spec) where

import Brokkr.Packet.Common
import Brokkr.Packet.Encode qualified as Encode
import Brokkr.Packet.ClientToServer.Play qualified as Client

import Utils.Setup
import Test.Syd
import Utils.Play

spec :: BrokkrSpec
spec = do
  withTestClient "Move1" $ do
    it "should accept movement packets without crashing" $ \client -> do
      let maxTime = 1_000
          maxTimeLarge = 10_000
      joinGame maxTime maxTimeLarge client

      -- TODO Query the current position of the client and adjust that
      -- TODO Get a free position first, then make this a prop test to move around in that area
      --      Then check that we receive SetCenterChunk, new chunks and UnloadChunk

      sendPacket_ "SetPlayerOnGround" maxTime client (Encode.Packet (Encode.EstimateMin 64) $ Client.SetPlayerOnGround OnGround)
      sendPacket_ "SetPlayerPosition" maxTime client (Encode.Packet (Encode.EstimateMin 64) $ Client.SetPlayerPosition (Position 0 80 0) OnGround)
      sendPacket_ "SetPlayerRotation" maxTime client (Encode.Packet (Encode.EstimateMin 64) $ Client.SetPlayerRotation (Rotation 15 80) OnGround)
      sendPacket_ "SetPlayerPositionAndRotation" maxTime client (Encode.Packet (Encode.EstimateMin 64) $ Client.SetPlayerPositionAndRotation (Position 0 30 0) (Rotation 20 50) OnGround)

      -- TODO Query the internal state of the server and check if we moved
