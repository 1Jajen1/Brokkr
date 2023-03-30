{-# LANGUAGE DerivingVia #-}
module Brokkr.PackedVector.Pack (
  Pack(..)
) where

import Data.Word
import Data.Bits
import Data.Int

-- | Type class for things that can be packed into 64 bit words
--
-- Mostly exists such that all integer types can be used easily
class Pack a where
  -- | Create a 64 bit word where the element is at a given index
  --
  -- The first argument is the bit size that should be used
  pack :: Int -> Int -> a -> Word64
  -- | Extract an element from a 64 bit word
  --
  -- The first argument is the bit size that should be used
  unpack :: Int -> Int -> Word64 -> a
  -- | Write an element into an existing 64 bit word
  --
  -- The first argument is the bit size that should be used
  packInto :: Int -> Int -> Word64 -> a -> Word64
  packInto bitSz wI w el =
    let off = wI * bitSz
        w' = w .&. complement (unsafeShiftL (mask bitSz) off)
    in w' .|. pack bitSz wI el
  {-# INLINE packInto #-}

newtype ViaIntegral a = VI a

instance Integral a => Pack (ViaIntegral a) where
  pack bits i (VI w) = unsafeShiftL (fromIntegral w .&. mask bits) (i * bits)
  {-# INLINE pack #-}
  unpack bits wI w = VI . fromIntegral $ unsafeShiftR w (wI * bits) .&. mask bits
  {-# INLINE unpack #-}

deriving via (ViaIntegral Word) instance Pack Word
deriving via (ViaIntegral Word64) instance Pack Word64
deriving via (ViaIntegral Word32) instance Pack Word32
deriving via (ViaIntegral Word16) instance Pack Word16
deriving via (ViaIntegral Word8) instance Pack Word8

deriving via (ViaIntegral Int) instance Pack Int
deriving via (ViaIntegral Int64) instance Pack Int64
deriving via (ViaIntegral Int32) instance Pack Int32
deriving via (ViaIntegral Int16) instance Pack Int16
deriving via (ViaIntegral Int8) instance Pack Int8

mask :: Int -> Word64
mask bitSz = (1 `unsafeShiftL` bitSz) - 1
{-# INLINE mask #-}
