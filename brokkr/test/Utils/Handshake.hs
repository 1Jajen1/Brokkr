{-# LANGUAGE DeriveAnyClass #-}
module Utils.Handshake (
  handshake
) where

import Brokkr.Packet.Encode qualified as Encode

import Brokkr.Packet.ClientToServer.Handshake qualified as Server
import Brokkr.Packet.ClientToServer.Handshake.Types.Internal qualified as Server

import Utils.Client
import Control.Exception

handshake :: Int -> Server.NextState -> TestClient -> IO ()
handshake maxTime next client = do
  res <- sendPacket maxTime client $ Encode.Packet (Encode.EstimateMin 100) $ Server.Handshake
        (Server.ProtocolVersion . fromIntegral $ clientProtocolVersion client)
        (Server.UnsafeServerAddress $ clientAddress client)
        (Server.ServerPort $ clientPort client)
        next
  case res of
    Just e -> throwIO $ HandshakeFailed e
    Nothing -> pure ()

newtype HandshakeFailed = HandshakeFailed SomeException
  deriving stock Show
  deriving anyclass Exception
