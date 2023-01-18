{-# LANGUAGE TemplateHaskell #-}
module Network.Packet.Server.Login (
  LoginPacket(..)
) where

import FlatParse.Basic
import Data.UUID
import Data.Text hiding (empty)
import Data.Int
import Data.Word
import Data.Coerce
import Util.Binary
import Network.Util.Packet
import Network.Util.MCString
import Network.Util.VarNum
import qualified Data.Vector.Storable as S

import Debug.Trace

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
