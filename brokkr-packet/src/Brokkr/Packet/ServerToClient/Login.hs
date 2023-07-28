{-# LANGUAGE TemplateHaskell #-}
module Brokkr.Packet.ServerToClient.Login (
  LoginPacket(..)
, module Brokkr.Packet.Common
, module Brokkr.Packet.ServerToClient.Login.Types
) where

import Brokkr.Packet.Binary
import Brokkr.Packet.Common
import Brokkr.Packet.TH
import Brokkr.Packet.ServerToClient.Login.Types
import Brokkr.Packet.SizePrefixed

import Data.Coerce
import qualified Data.Vector as V

import Language.Haskell.TH

data LoginPacket =
    Disconnect !Chat
  | EncryptionRequest !ServerId !PublicKey !VerifyToken
  | LoginSuccess !UserUUID !Username !(V.Vector Property)
  | SetCompression !CompressionThreshold
  | LoginPluginRequest !MessageId !RequestData
  deriving stock Show

-- Why? No clue
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/9813
$(return [])

instance ToBinary LoginPacket where
  put = $(mkPacketBuilder ''LoginPacket [
      ('LoginSuccess, do
        nms@[nuuid, nuname, nprops] <- sequence [newName "uuid", newName "uname", newName "props"]
        match (conP 'LoginSuccess (fmap varP nms)) (normalB [|
          put $(varE nuuid) <> put $(varE nuname) <> put (SizePrefixed @VarInt $(varE nprops))
          |]) [])
    ])
  {-# INLINE put #-}

instance FromBinary LoginPacket where
  get = $(mkPacketParser ''LoginPacket [
      ('LoginSuccess, [| LoginSuccess <$> get <*> get <*> (coerce <$> get @(SizePrefixed VarInt (V.Vector Property))) |])
    ])
