module MovementSpec (spec) where

import Utils.Setup
import Test.Syd
import Utils.Setup
import Utils.Play
import qualified Network.Packet.Server.Play as Client
import Util.Position
import Util.Rotation

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

      sendPacket_ "SetPlayerOnGround" maxTime client 64 (Client.SetPlayerOnGround Falling)
      sendPacket_ "SetPlayerPosition" maxTime client 64 (Client.SetPlayerPosition (Position 0 80 0) Falling)
      sendPacket_ "SetPlayerRotation" maxTime client 64 (Client.SetPlayerRotation (Rotation 15 80) Falling)
      sendPacket_ "SetPlayerPositionAndRotation" maxTime client 64 (Client.SetPlayerPositionAndRotation (Position 0 30 0) (Rotation 20 50) Falling)

      -- TODO Query the internal state of the server and check if we moved
