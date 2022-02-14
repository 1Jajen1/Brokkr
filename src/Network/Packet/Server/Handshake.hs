{-# LANGUAGE PatternSynonyms #-}
module Network.Packet.Server.Handshake (
  HandshakePacket(..)
, ProtocolVersion(..)
, ServerAddress(..)
, ServerPort(..)
, NextState
, pattern Status
, pattern Login
) where

import Data.Int
import Data.Text (Text)
import Util.Binary
import Network.Util.VarNum
import Data.Word
import Network.Util.MCString
import FlatParse.Basic

data HandshakePacket = Handshake ProtocolVersion ServerAddress ServerPort NextState
  deriving stock Show

instance FromBinary HandshakePacket where
  get = get @VarInt >>= \case
    VarInt 0 -> Handshake <$> get <*> get <*> get <*> get
    _ -> empty
  {-# INLINE get #-}

instance ToBinary HandshakePacket where
  put (Handshake pv sa sp n) = put (VarInt 0) <> put pv <> put sa <> put sp <> put n

newtype ProtocolVersion = PV Int32
  deriving (FromBinary, ToBinary) via VarInt

instance Show ProtocolVersion where
  show (PV i) = prettyVersion i

newtype ServerAddress = SA Text
  deriving (FromBinary, ToBinary) via MCString

instance Show ServerAddress where
  show (SA t) = show t -- TODO

newtype ServerPort = SP Word16
  deriving newtype (FromBinary, ToBinary)

instance Show ServerPort where
  show (SP i) = show i -- TODO

newtype NextState = NS Int32
  deriving (FromBinary, ToBinary) via VarInt

instance Show NextState where
  show = \case
    Status -> "Status"
    Login  -> "Login"

pattern Status :: NextState
pattern Status = NS 1

pattern Login :: NextState
pattern Login = NS 2

{-# COMPLETE Status, Login #-}

prettyVersion :: Int32 -> String
prettyVersion 757 = "1.18.1"
prettyVersion n = show n
