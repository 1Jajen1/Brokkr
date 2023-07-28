{-# LANGUAGE DataKinds #-}
module Brokkr.Packet.ClientToServer.Login.Types (
  SharedSecret(..)
, ResponseData(..)
) where

import Brokkr.Packet.Binary
import Brokkr.Packet.SizePrefixed

import Data.ByteString (ByteString)

import FlatParse.Basic qualified as Flatparse

newtype SharedSecret = SharedSecret ByteString
  deriving (ToBinary, FromBinary) via SizePrefixed VarInt ByteString

instance Show SharedSecret where
  showsPrec prec (SharedSecret bs) = showParen (prec > 10) $
    showString "SharedSecret " . showsPrec 11 (HexBS bs)

newtype ResponseData = ResponseData ByteString
  deriving newtype ToBinary

instance FromBinary ResponseData where
  get = ResponseData <$> Flatparse.takeRest
  {-# INLINE get #-}

instance Show ResponseData where
  showsPrec prec (ResponseData bs) = showParen (prec > 10) $
    showString "ResponseData " . showsPrec 11 (HexBS bs)

