{-# LANGUAGE CPP, DerivingStrategies #-}
module Brokkr.NBT.ByteOrder (
  Int32BE(..)
, Int64BE(..)
, arrSwapBE32
, arrSwapBE64
, unsafeArrSwapBE32
, unsafeArrSwapBE64
) where

import Control.Monad.Primitive

import Data.Coerce
import Data.Int
import Data.Primitive

import Data.Vector.Generic.Mutable qualified as GV
import Data.Vector.Storable qualified as S

import Data.Word

import Foreign.C.Types (CSize(..))
import Foreign.Storable

import GHC.ForeignPtr

import Unsafe.Coerce qualified as Unsafe

newtype Int32BE = Int32BE Int32
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral)

instance Storable Int32BE where
  sizeOf _ = 4
  {-# INLINE sizeOf #-}
  alignment _ = 4
  {-# INLINE alignment #-}
  poke ptr (Int32BE i) = poke (coerce ptr) $ swapBE32 i
  {-# INLINE poke #-}
  peek = fmap (Int32BE . swapBE32) . peek . coerce
  {-# INLINE peek #-}

newtype Int64BE = Int64BE Int64
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral)

instance Storable Int64BE where
  sizeOf _ = 8
  {-# INLINE sizeOf #-}
  alignment _ = 8
  {-# INLINE alignment #-}
  poke ptr (Int64BE i) = poke (coerce ptr) $ swapBE64 i
  {-# INLINE poke #-}
  peek = fmap (Int64BE . swapBE64) . peek . coerce
  {-# INLINE peek #-}

-- Swap byte order if and only if the native byte order is not big endian
swapBE32 :: Int32 -> Int32
{-# INLINE swapBE32 #-}
swapBE64 :: Int64 -> Int64
{-# INLINE swapBE64 #-}
-- Copies the vector with all bytes swapped
-- Noop on big endian systems
arrSwapBE32 :: PrimMonad m => S.Vector Int32BE -> m (S.Vector Int32)
{-# INLINE arrSwapBE32 #-}
arrSwapBE64 :: PrimMonad m => S.Vector Int64BE -> m (S.Vector Int64)
{-# INLINE arrSwapBE64 #-}
-- Swap bytes in place. Unsafe if the original vector is used again
-- Noop on big endian systems
-- More beneficial on small to medium sized vectors, on large vectors the additional allocation
-- is a fraction of the cost of byteswapping. This is because the cost of
-- allocation is almost constant after a given threshold whereas the byteswapping is linear.
unsafeArrSwapBE32 :: PrimMonad m => S.Vector Int32BE -> m (S.Vector Int32)
{-# INLINE unsafeArrSwapBE32 #-}
unsafeArrSwapBE64 :: PrimMonad m => S.Vector Int64BE -> m (S.Vector Int64)
{-# INLINE unsafeArrSwapBE64 #-}

#ifdef WORDS_BIGENDIAN

swapBE32 = id
swapBE64 = id

arrSwapBE32 v = pure $ Unsafe.unsafeCoerce v
arrSwapBE64 v = pure $ Unsafe.unsafeCoerce v

unsafeArrSwapBE32 v = pure $ Unsafe.unsafeCoerce v
unsafeArrSwapBE64 v = pure $ Unsafe.unsafeCoerce v

#else

swapBE32 = fromIntegral . byteSwap32 . fromIntegral
swapBE64 = fromIntegral . byteSwap64 . fromIntegral

arrSwapBE32 v = do
  to <- GV.unsafeNew sz >>= S.unsafeFreeze
  let !(ForeignPtr toAddr _, _) = S.unsafeToForeignPtr0 to
  seq (c_vec_bswap32 (Ptr fromAddr) (Ptr toAddr) (fromIntegral sz)) $ pure to
  where !(ForeignPtr fromAddr _, sz) = S.unsafeToForeignPtr0 v
arrSwapBE64 v = do
  to <- GV.unsafeNew sz >>= S.unsafeFreeze
  let !(ForeignPtr toAddr _, _) = S.unsafeToForeignPtr0 to
  seq (c_vec_bswap64 (Ptr fromAddr) (Ptr toAddr) (fromIntegral sz)) $ pure to
  where !(ForeignPtr fromAddr _, sz) = S.unsafeToForeignPtr0 v

unsafeArrSwapBE32 v = seq (c_vec_bswap32 (Ptr addr) (Ptr addr) (fromIntegral sz)) . pure $ Unsafe.unsafeCoerce v
  where !(ForeignPtr addr _, sz) = S.unsafeToForeignPtr0 v
unsafeArrSwapBE64 v = seq (c_vec_bswap64 (Ptr addr) (Ptr addr) (fromIntegral sz)) . pure $ Unsafe.unsafeCoerce v
  where !(ForeignPtr addr _, sz) = S.unsafeToForeignPtr0 v

foreign import ccall unsafe "vec_bswap32" c_vec_bswap32 :: Ptr Int32BE -> Ptr Int32 -> CSize -> ()
foreign import ccall unsafe "vec_bswap64" c_vec_bswap64 :: Ptr Int64BE -> Ptr Int64 -> CSize -> ()

#endif
