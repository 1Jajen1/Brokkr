{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Brokkr.Packet.ClientToServer.Login (
  LoginPacket(..)
, module Brokkr.Packet.ClientToServer.Login.Types
, module Brokkr.Packet.Common
) where

import Brokkr.Packet.Binary
import Brokkr.Packet.ClientToServer.Login.Types
import Brokkr.Packet.Common
import Brokkr.Packet.TH

data LoginPacket =
    LoginStart !Username !(Maybe UserUUID)
  | EncryptionResponse !SharedSecret !VerifyToken
  | LoginPluginResponse !MessageId !(Maybe ResponseData)
  deriving stock Show

-- Why? No clue
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/9813
$(return [])

instance ToBinary LoginPacket where
  put = $(mkPacketBuilder ''LoginPacket [])
  {-# INLINE put #-}

instance FromBinary LoginPacket where
  get = $(mkPacketParser ''LoginPacket [])
