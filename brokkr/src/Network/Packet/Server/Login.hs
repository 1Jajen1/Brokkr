{-# LANGUAGE TemplateHaskell #-}
module Network.Packet.Server.Login (
  LoginPacket(..)
) where

import Data.UUID
import Data.Text hiding (empty)
import Data.Coerce
import Util.Binary
import Network.Util.Packet
import Network.Util.MCString

data LoginPacket =
    LoginStart Text (Maybe UUID)
  deriving stock Show

instance FromBinary LoginPacket where
  get = $$(mkPacketParser [
      [|| loginStart ||]
    ])
    where
      loginStart = do
        uName <- coerce <$> get @MCString
        uuid <- get >>= \case
          True -> Just <$> get
          False -> pure Nothing
        pure $ LoginStart uName uuid
