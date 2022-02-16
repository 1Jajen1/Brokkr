{-# LANGUAGE MagicHash #-}
module Util.Word24 (
  Word24(..)
, byteSwap24
) where

import GHC.Exts

newtype Word24 = Word24 Word
  deriving newtype (Eq, Show, Enum, Ord, Num, Real, Integral)

byteSwap24 :: Word24 -> Word24
byteSwap24 (Word24 (W# w)) = Word24 $ W# (byteSwap24# w)

byteSwap24# :: Word# -> Word#
byteSwap24# w =
  let b1 = uncheckedShiftL# (w `and#` 0x0000ff##) 16#
      b2 = w `and#` 0x00FF00##
      b3 = uncheckedShiftRL# (w `and#` 0xff0000##) 16#
  in b1 `or#` b2 `or#` b3
