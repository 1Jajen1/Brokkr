{-# LANGUAGE TemplateHaskell #-}
module Network.Packet.Server.Login (
  LoginPacket(..)
) where

import Data.Text
import Data.Coerce
import Util.Binary
import Network.Util.Packet
import Network.Util.MCString

data LoginPacket =
    LoginStart Text
  deriving stock Show

instance FromBinary LoginPacket where
  get = $$(mkPacketParser [
      [|| loginStart ||]
    ])
    where loginStart = LoginStart . coerce <$> get @MCString
