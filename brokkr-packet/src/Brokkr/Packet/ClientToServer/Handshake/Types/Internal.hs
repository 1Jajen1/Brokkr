{-# LANGUAGE DataKinds #-}
module Brokkr.Packet.ClientToServer.Handshake.Types.Internal (
  ProtocolVersion(..)
, ServerAddress(..), mkServerAddress
, ServerPort(..)
, NextState(..)
) where

import Brokkr.Packet.Binary
import Brokkr.Packet.MCString

import Data.Coerce
import Data.Int
import Data.Text (Text)
import Data.Word

import FlatParse.Basic qualified as Flatparse

newtype ProtocolVersion = ProtocolVersion Int32
  deriving stock Show
  deriving (FromBinary, ToBinary) via VarInt

newtype ServerAddress = UnsafeServerAddress { unServerAddress :: Text }
  deriving stock Show
  deriving (FromBinary, ToBinary) via MCString 255

mkServerAddress :: Text -> Maybe ServerAddress
mkServerAddress = coerce . mkMCString @255

newtype ServerPort = ServerPort Word16
  deriving stock Show
  deriving newtype (FromBinary, ToBinary)

data NextState = Status | Login
  deriving stock Show

instance ToBinary NextState where
  put Status = put (VarInt 1)
  put Login =  put (VarInt 2)
  {-# INLINE put #-}

instance FromBinary NextState where
  with f = with @VarInt $ \case
    1 -> f Status
    2 -> f Login
    n -> Flatparse.err $ InvalidEnumValue "NextState" (fromIntegral n)
  {-# INLINE with #-}
