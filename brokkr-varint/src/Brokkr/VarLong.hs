{-# LANGUAGE DataKinds #-}
module Brokkr.VarLong (
  VarLong(..)
, Decode.VarIntDecodeError
, withVarLong
, putVarLong
) where

import Brokkr.VarInt.Decode qualified as Decode
import Brokkr.VarInt.Encode qualified as Encode

import Data.Bits
import Data.Int

import FlatParse.Basic qualified as Flatparse

import Mason.Builder qualified as Mason

-- | VarLong as per the minecraft protocols specifications
--
-- The format is effective for small non-negative numbers and used a few times in places
-- in the minecraft protocol.
--
-- It is effectively 'VarInt' but for 64-bit numbers.
--
-- Variable length encoding for numbers. The most significant bit indicates
-- whether another byte follows. The remaining 7 bits encode the number.
-- The least significant bits are written first.
--
-- 'VarLong's will never exceed 10 bytes.
--
-- Examples: 127 ==> 0x7f; 128 ==> 0x8001; 255 ==> 0xff01; -1 ==> 0xffffffffffffffffff0f
newtype VarLong = VarLong Int64
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

-- | Decode a 'VarLong' in continuation passing style to avoid allocations.
--
-- Allows a custom error type, but requires handling 'Decode.VarIntDecodeError'
-- Currently only a 'VarLong' with more than 10 bytes leads to such an error.
-- 
-- For more low level control 'Decode.withVarInt' can be used instead. 
--
-- Uses 'flatparse' as it offers low overhead parsing often required for parsing
-- network packets. Visit 'VarInt.Decode' for lower level parser independent code.
withVarLong :: (Decode.VarIntDecodeError -> e) -> (VarLong -> Flatparse.ParserT st e a) -> Flatparse.ParserT st e a
{-# INLINE withVarLong #-}
withVarLong handleVarIntError f = Decode.withVarInt @10 handleVarIntError (f . fromIntegral)

-- | Encode a 'VarLong' using 'Mason.Builder'
--
-- For more low level control or for builder independent code visit 'VarInt.Encode'
putVarLong :: VarLong -> Mason.Builder
{-# INLINE putVarLong #-}
putVarLong (VarLong i) = Encode.unsafePutVarInt @10 $ 0xFFFFFFFFFFFFFFFF .&. fromIntegral i
