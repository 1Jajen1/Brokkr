{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
module Utils.Login (
  login
) where

import Utils.Client
import Utils.Handshake
import Control.Exception

import Brokkr.Packet.Encode qualified as Encode

import Brokkr.Packet.ClientToServer.Handshake (pattern Login)

import Brokkr.Packet.ServerToClient.Login qualified as Client
import Brokkr.Packet.ClientToServer.Login qualified as Server
import Brokkr.Packet.Common.Internal (Username(..))
import Control.Monad.Fix

-- Login a client. Throws on failure
-- Expects the client to be in handshake state
login :: Int -> TestClient -> IO ()
login maxTime client = do
  handshake maxTime Login client
  login_ maxTime client

-- | Login a client. Throws on failure
-- Expects the client to already be in Login protocol state
-- Brings the client <-> server connection into play state
-- Only consumes up to the LoginSuccess packet, not the Login (play) packet
login_ :: Int -> TestClient -> IO ()
login_ maxTime client = do
  -- Send the login start
  sendPacket maxTime client
    (Encode.Packet
      (Encode.EstimateMin 100)
      (Server.LoginStart (UnsafeUsername $ clientName client) Nothing)) >>= \case
    Just e -> throwIO $ FailedLogin e
    Nothing -> pure ()
  
  -- read until we get either a login success or a set compression
  -- TODO Later add encryption here
  -- We don't care if SetCompression or encryption requests are made multiple times. Why?
  --  Well the effects of these packets are effective immediately, so a repeated packet will
  --  fail to parse.
  fix $ \go ->
    -- handle packet
    readPacket maxTime client (\cs es -> \case
      Client.SetCompression (Client.CompressionThreshold threshold) -> (Encode.UseCompression $ fromIntegral threshold, es)
      _ -> (cs, es)) >>= \case
      Left e -> throwIO $ FailedLogin e
      Right Client.SetCompression{} -> go
      Right Client.LoginSuccess{} -> pure ()
      Right Client.Disconnect{} -> throwIO LoginDisconnected
      Right Client.EncryptionRequest{} -> throwIO UnexpectedEncryption

data FailedLogin =
    FailedLogin SomeException
  | LoginDisconnected
  | UnexpectedEncryption -- We don't encrypt here, thats just extra work we don't need for tests
  deriving stock Show
  deriving anyclass Exception
