{-# LANGUAGE CPP, DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
-- Disables the warning for unsafeArrSwapBE. The 'Coercible' constraint is technically
-- redundant, but it ensures the byte size is correct.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Brokkr.NBT.ByteOrder (
  Int32BE(..)
, Int64BE(..)
, Swapped
, arrSwapBE32
, arrSwapBE64
, unsafeArrSwapBE32
, unsafeArrSwapBE64
, toBE16
, toBE32
, toBE64
) where

import Control.Monad.ST.Strict (runST)

import Data.Coerce
import Data.Int
import Data.Kind
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

type family Swapped (a :: Type) :: Type
type instance Swapped Int32 = Int32BE
type instance Swapped Int64 = Int64BE
type instance Swapped Int32BE = Int32
type instance Swapped Int64BE = Int64

-- Copies the vector with all bytes swapped
-- Noop on big endian systems
arrSwapBE32 :: (Coercible a Int32, Storable a, Storable (Swapped a)) => S.Vector a -> S.Vector (Swapped a)
{-# INLINE arrSwapBE32 #-}
arrSwapBE64 :: (Coercible a Int64, Storable a, Storable (Swapped a)) => S.Vector a -> S.Vector (Swapped a)
{-# INLINE arrSwapBE64 #-}
-- Swap bytes in place. Unsafe if the original vector is used again
-- Noop on big endian systems
-- More beneficial on small to medium sized vectors, on large vectors the additional allocation
-- is a fraction of the cost of byteswapping. This is because the cost of
-- allocation is almost constant after a given threshold whereas the byteswapping is linear.
unsafeArrSwapBE32 :: (Coercible a Int32, Storable a) => S.Vector a -> S.Vector (Swapped a)
{-# INLINE unsafeArrSwapBE32 #-}
unsafeArrSwapBE64 :: (Coercible a Int64, Storable a) => S.Vector a -> S.Vector (Swapped a)
{-# INLINE unsafeArrSwapBE64 #-}

-- Convert a word from host endianess into big endian
toBE16 :: Int16 -> Int16
{-# INLINE toBE16 #-}
toBE32 :: Int32 -> Int32
{-# INLINE toBE32 #-}
toBE64 :: Int64 -> Int64
{-# INLINE toBE64 #-}

#ifdef WORDS_BIGENDIAN

swapBE32 = id
swapBE64 = id

arrSwapBE32 = Unsafe.unsafeCoerce
arrSwapBE64 = Unsafe.unsafeCoerce

unsafeArrSwapBE32 = Unsafe.unsafeCoerce
unsafeArrSwapBE64 = Unsafe.unsafeCoerce

toBE16 = id
toBE32 = id
toBE64 = id

#else

swapBE32 = fromIntegral . byteSwap32 . fromIntegral
swapBE64 = fromIntegral . byteSwap64 . fromIntegral

arrSwapBE32 v = runST $ do
  to <- GV.unsafeNew sz >>= S.unsafeFreeze
  let !(ForeignPtr toAddr _, _) = S.unsafeToForeignPtr0 to
  seq (c_vec_bswap32 (Ptr fromAddr) (Ptr toAddr) (fromIntegral sz)) $ pure to
  where !(ForeignPtr fromAddr _, sz) = S.unsafeToForeignPtr0 v
arrSwapBE64 v = runST $ do
  to <- GV.unsafeNew sz >>= S.unsafeFreeze
  let !(ForeignPtr toAddr _, _) = S.unsafeToForeignPtr0 to
  seq (c_vec_bswap64 (Ptr fromAddr) (Ptr toAddr) (fromIntegral sz)) $ pure to
  where !(ForeignPtr fromAddr _, sz) = S.unsafeToForeignPtr0 v

unsafeArrSwapBE32 v = seq (c_vec_bswap32 (Ptr addr) (Ptr addr) (fromIntegral sz)) $ Unsafe.unsafeCoerce v
  where !(ForeignPtr addr _, sz) = S.unsafeToForeignPtr0 v
unsafeArrSwapBE64 v = seq (c_vec_bswap64 (Ptr addr) (Ptr addr) (fromIntegral sz)) $ Unsafe.unsafeCoerce v
  where !(ForeignPtr addr _, sz) = S.unsafeToForeignPtr0 v

foreign import ccall unsafe "vec_bswap32" c_vec_bswap32 :: Ptr Int32BE -> Ptr Int32 -> CSize -> ()
foreign import ccall unsafe "vec_bswap64" c_vec_bswap64 :: Ptr Int64BE -> Ptr Int64 -> CSize -> ()

toBE16 = fromIntegral . byteSwap16 . fromIntegral
toBE32 = fromIntegral . byteSwap32 . fromIntegral
toBE64 = fromIntegral . byteSwap64 . fromIntegral

#endif
