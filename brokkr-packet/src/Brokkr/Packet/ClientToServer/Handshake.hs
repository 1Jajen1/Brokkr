{-# LANGUAGE DataKinds #-}
module Brokkr.Packet.ClientToServer.Handshake (
  HandshakePacket(..)
, module Brokkr.Packet.ClientToServer.Handshake.Types
) where

import Brokkr.Packet.Binary

import Brokkr.Packet.ClientToServer.Handshake.Types

import FlatParse.Basic qualified as Flatparse

data HandshakePacket =
  Handshake !ProtocolVersion !ServerAddress !ServerPort !NextState
  deriving stock Show

instance ToBinary HandshakePacket where
  put (Handshake pv sa sp next) =
       put (VarInt 0) <> put pv <> put sa <> put sp <> put next
  {-# INLINE put #-}

instance FromBinary HandshakePacket where
  get = with @VarInt $ \case
    0 -> Handshake <$> get <*> get <*> get <*> get
    n -> Flatparse.err $ InvalidPacketId "Handshake" (fromIntegral n)
  {-# INLINE get #-}
