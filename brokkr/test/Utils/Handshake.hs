{-# LANGUAGE DeriveAnyClass #-}
module Utils.Handshake (
  handshake
) where

import qualified Network.Packet.Server.Handshake as Server

import Utils.Client
import Control.Exception

handshake :: Int -> Server.NextState -> TestClient -> IO ()
handshake maxTime next client = do
  res <- sendPacket maxTime client 100 $ Server.Handshake
        (Server.PV . fromIntegral $ clientProtocolVersion client)
        (Server.SA $ clientAddress client)
        (Server.SP $ clientPort client)
        next
  case res of
    Just e -> throwIO $ HandshakeFailed e
    Nothing -> pure ()

newtype HandshakeFailed = HandshakeFailed SomeException
  deriving stock Show
  deriving anyclass Exception
