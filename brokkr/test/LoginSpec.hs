{-# LANGUAGE OverloadedStrings #-}
module LoginSpec (spec) where

import Test.Syd
import Test.Syd.Hedgehog ()

import Utils.Setup
import Utils.Client
import Utils.Handshake

import Network.Protocol

import Network.Packet.Server.Handshake qualified as Server
import Network.Packet.Server.Login     qualified as Server

import Network.Packet.Client.Login qualified as Client
import Network.Packet.Client.Play  qualified as Client

spec :: BrokkrSpec
spec = do
  withTestClient "Login1" $ do
    it "should reach the play packet" $ \(client :: TestClient) -> do
      -- Login (Play) and the chunks that follow are a lot of data
      -- and frequently take > 1ms to arrive
      -- I may need to tweak this is the pressure on the server grows
      let maxTime = 10_000
      handshake maxTime Server.Login client

      sendPacket_ "LoginStart" maxTime client 10 $ Server.LoginStart (clientName client) Nothing

      expectSetCompression maxTime client

      readPacket_ "LoginSuccess" maxTime client $ \case
        Client.LoginSuccess _uid uName -> do
          uName `shouldBe` clientName client
        p -> unexpectedPacket "LoginSuccess" p

      readPacket_ "Login (Play)" maxTime client $ \case
        Client.Login _ -> do
          pure ()
        p -> unexpectedPacket "Login (Play)" p
  
  -- The test server is linked to the thread that created it, so
  -- a server crash will bring down the testsuite (as it should)

  -- Network threads crashing should be expected and should never
  -- crash the server.
  -- TODO Validate that there is no trace of the connections left.
  -- Take the universe object from the server and check that it
  -- has no connection for the clients named Login2..LoginX
  -- I currently cannot really do that though because I don't
  -- yet store enough info.
  -- I have no UUID|PlayerName -> EntityId lookup yet

  describe "partial login sequences" $ do
    withTestClient "Login2" $ do
      it "should not crash the server after handshake" $ \client -> do
        let maxTime = 1_000
        handshake maxTime Server.Login client
    withTestClient "Login3" $ do
      it "should not crash the server after login start" $ \client -> do
        let maxTime = 1_000
        handshake maxTime Server.Login client
        sendPacket_ "LoginStart" maxTime client 10 $ Server.LoginStart (clientName client) Nothing

-- This expects the test server to have compression setup, which is fine for now I guess...
expectSetCompression :: Int -> TestClient -> IO ()
expectSetCompression maxTime client = readPacketWithProtocol_ "SetCompression" maxTime client (\p@(Protocol _ encr) -> \case
  Client.SetCompression threshold -> Protocol (Threshold threshold) encr
  _ -> p
  ) $ \case
  Client.SetCompression _ -> pure ()
  p -> unexpectedPacket "SetCompression" p
