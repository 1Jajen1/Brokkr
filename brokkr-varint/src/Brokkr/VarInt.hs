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

-- | VarInt as per the minecraft protocols specifications
--
-- The format is effective for small non-negative numbers and used in quite a lot of places
-- in the minecraft protocol.
--
-- Variable length encoding for numbers. The most significant bit indicates
-- whether another byte follows. The remaining 7 bits encode the number.
-- The least significant bits are written first.
--
-- 'VarInt's will never exceed 5 bytes.
--
-- Examples: 127 ==> 0x7f; 128 ==> 0x8001; 255 ==> 0xff01; -1 ==> 0xffffffff0f
newtype VarInt = VarInt Int32
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

-- | Decode a 'VarInt' in continuation passing style to avoid allocations.
--
-- Allows a custom error type, but requires handling 'Decode.VarIntDecodeError'
-- Currently only a 'VarInt' with more than 5 bytes leads to such an error.
-- 
-- For more low level control 'Decode.withVarInt' can be used instead. 
--
-- Uses 'flatparse' as it offers low overhead parsing often required for parsing
-- network packets. Visit 'VarInt.Decode' for lower level parser independent code.
withVarInt :: (Decode.VarIntDecodeError -> e) -> (VarInt -> Flatparse.ParserT st e a) -> Flatparse.ParserT st e a
{-# INLINE withVarInt #-}
withVarInt handleVarIntError f = Decode.withVarInt @5 handleVarIntError (f . fromIntegral)

-- | Encode a 'VarInt' using 'Mason.Builder'
--
-- For more low level control or for builder independent code visit 'VarInt.Encode'
putVarInt :: VarInt -> Mason.Builder
{-# INLINE putVarInt #-}
putVarInt (VarInt i) = Encode.unsafePutVarInt @5 $ 0xFFFFFFFF .&. fromIntegral i
