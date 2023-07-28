{-# LANGUAGE DataKinds #-}
module Brokkr.VarInt (
  VarInt(..)
, Decode.VarIntDecodeError
, withVarInt
, putVarInt
) where

import Brokkr.VarInt.Decode qualified as Decode
import Brokkr.VarInt.Encode qualified as Encode

import Data.Bits
import Data.Int

import FlatParse.Basic qualified as Flatparse

import Mason.Builder qualified as Mason

newtype VarInt = VarInt Int32
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

withVarInt :: (Decode.VarIntDecodeError -> e) -> (VarInt -> Flatparse.ParserT st e a) -> Flatparse.ParserT st e a
{-# INLINE withVarInt #-}
withVarInt handleVarIntError f = Decode.withVarInt @5 handleVarIntError (f . fromIntegral)

putVarInt :: VarInt -> Mason.Builder
{-# INLINE putVarInt #-}
putVarInt (VarInt i) = Encode.unsafePutVarInt @5 $ 0xFFFFFFFF .&. fromIntegral i
