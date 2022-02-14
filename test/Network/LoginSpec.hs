{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans -Wno-missing-export-lists #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.LoginSpec where

import Test.Syd
import Network.Effect.Network.Pure (runNetwork)
import Effectful
import Effectful.State.Static.Local (evalState)
import Network.Handler.Login
import Data.ByteString
import Network.Utils (encodePackets)
import qualified Network.Packet.Server.Handshake as S
import Util.Binary (put)
import qualified Network.Packet.Server.Login as S
import qualified Network.Packet.Client.Login as C
import qualified Data.UUID.V3 as UUID
import qualified Data.UUID as UUID
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

-- TODO Extract the pattern of generating packets and running the network effect
-- TODO Add generators for packet inputs
--      Also generate faults and random timings
spec :: Spec
spec = do
  describe "loginHandler" $ do
    it "should properly handle login" $ do
      let name = "Jannis"
          recPackets = encodePackets [
              [ put $ S.LoginStart name ]
            ]
          expectedUid = UUID.generateNamed UUID.nil $ BS.unpack $ T.encodeUtf8 $ name
          expectedPackets = encodePackets [
              [ put $ C.LoginSuccess expectedUid name ]
            ]
          ((uid, uname), sentPackets) = runPureEff $ runNetwork recPackets $ evalState (mempty @ByteString) $ loginProtocol
      uid `shouldBe` expectedUid
      uname `shouldBe` name
      sentPackets `shouldBe` expectedPackets
