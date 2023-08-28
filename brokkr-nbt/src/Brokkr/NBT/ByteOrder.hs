{-# LANGUAGE CPP, DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
-- Disables the warning for unsafeArrSwapBE. The 'Coercible' constraint is technically
-- redundant, but it ensures the byte size is correct.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Brokkr.NBT.ByteOrder (
  BigEndian(..)
, arrSwapBE
, unsafeArrSwapBE
) where

import Control.Monad.ST.Strict (runST)

import Data.Coerce
import Data.Int
import Data.Kind

import Data.Vector.Generic.Mutable qualified as GV
import Data.Vector.Storable qualified as S

import Data.Word

import Foreign.C.Types (CSize(..))
import Foreign.Storable

import GHC.ForeignPtr
import GHC.Exts
import GHC.Word
import GHC.Float

import Unsafe.Coerce qualified as Unsafe

newtype BigEndian a = BE a
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

class ToWord a where
  toWord :: a -> Word
  fromWord :: Word -> a

instance ToWord Int16 where
  toWord = fromIntegral
  {-# INLINE toWord #-}
  fromWord = fromIntegral
  {-# INLINE fromWord #-}
instance ToWord Int32 where
  toWord = fromIntegral
  {-# INLINE toWord #-}
  fromWord = fromIntegral
  {-# INLINE fromWord #-}
instance ToWord Int64 where
  toWord = fromIntegral
  {-# INLINE toWord #-}
  fromWord = fromIntegral
  {-# INLINE fromWord #-}
instance ToWord Float where
  toWord = fromIntegral . castFloatToWord32
  {-# INLINE toWord #-}
  fromWord = castWord32ToFloat . fromIntegral
  {-# INLINE fromWord #-}
instance ToWord Double where
  toWord = fromIntegral . castDoubleToWord64
  {-# INLINE toWord #-}
  fromWord = castWord64ToDouble . fromIntegral
  {-# INLINE fromWord #-}

instance (ToWord a, Storable a) => Storable (BigEndian a) where
  sizeOf _ = sizeOf (undefined :: a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: a)
  {-# INLINE alignment #-}
  peek ptr = case sizeOf (undefined :: a) of
    1 -> BE <$> peek (coerce ptr)
    2 -> peek @Word16 (coerce ptr) >>= \(W16# w) -> pure (BE . fromWord $ W# (byteSwap16# (word16ToWord# w)))
    4 -> peek @Word32 (coerce ptr) >>= \(W32# w) -> pure (BE . fromWord $ W# (byteSwap32# (word32ToWord# w)))
    8 -> peek @Word64 (coerce ptr) >>= \(W64# w) -> pure (BE . fromWord $ W# (word64ToWord# (byteSwap64# w)))
    _ -> error "Unknown BigEndian word size!"
  {-# INLINE peek #-}
  poke ptr (BE a) = case sizeOf (undefined :: a) of
    1 -> poke (coerce ptr) a
    2 -> poke (coerce ptr) $ W16# (wordToWord16# (byteSwap16# w))
    4 -> poke (coerce ptr) $ W32# (wordToWord32# (byteSwap32# w))
    8 -> poke (coerce ptr) $ W64# ((byteSwap64# (wordToWord64# w)))
    _ -> error "Unknown BigEndian word size!"
    where
      !(W# w) = toWord a
  {-# INLINE poke #-}

type family Swap a :: Type where
  Swap (BigEndian a) = a
  Swap a = BigEndian a

-- | Swap to and from big endian
--
-- Noop on big endian systems
arrSwapBE :: forall a b . Storable a => Storable b => Swap a ~ b => S.Vector a -> S.Vector b
{-# INLINE arrSwapBE #-}
-- | Swap to and from big endian **in place**
--
-- Noop on big endian systems
unsafeArrSwapBE :: forall a b . Storable a => Storable b => Swap a ~ b => S.Vector a -> S.Vector b
{-# INLINE unsafeArrSwapBE #-}

#ifdef WORDS_BIGENDIAN

arrSwapBE = Unsafe.unsafeCoerce

unsafeArrSwapBE = Unsafe.unsafeCoerce

#else

arrSwapBE v = case sizeOf (undefined :: a) of
  1 -> Unsafe.unsafeCoerce v
  2 -> runST $ do
    to <- GV.unsafeNew sz >>= S.unsafeFreeze
    let !(ForeignPtr toAddr _, _) = S.unsafeToForeignPtr0 to
    seq (c_vec_bswap16 (Ptr fromAddr) (Ptr toAddr) (fromIntegral sz)) $ pure to
    where !(ForeignPtr fromAddr _, sz) = S.unsafeToForeignPtr0 v
  4 -> runST $ do
    to <- GV.unsafeNew sz >>= S.unsafeFreeze
    let !(ForeignPtr toAddr _, _) = S.unsafeToForeignPtr0 to
    seq (c_vec_bswap32 (Ptr fromAddr) (Ptr toAddr) (fromIntegral sz)) $ pure to
    where !(ForeignPtr fromAddr _, sz) = S.unsafeToForeignPtr0 v
  8 -> runST $ do
    to <- GV.unsafeNew sz >>= S.unsafeFreeze
    let !(ForeignPtr toAddr _, _) = S.unsafeToForeignPtr0 to
    seq (c_vec_bswap64 (Ptr fromAddr) (Ptr toAddr) (fromIntegral sz)) $ pure to
    where !(ForeignPtr fromAddr _, sz) = S.unsafeToForeignPtr0 v
  _ -> error "unknown size"

unsafeArrSwapBE v = case sizeOf (undefined :: a) of
  1 -> Unsafe.unsafeCoerce v
  2 -> seq (c_vec_bswap16 (Ptr addr) (Ptr addr) (fromIntegral sz)) $ Unsafe.unsafeCoerce v
    where !(ForeignPtr addr _, sz) = S.unsafeToForeignPtr0 v
  4 -> seq (c_vec_bswap32 (Ptr addr) (Ptr addr) (fromIntegral sz)) $ Unsafe.unsafeCoerce v
    where !(ForeignPtr addr _, sz) = S.unsafeToForeignPtr0 v
  8 -> seq (c_vec_bswap64 (Ptr addr) (Ptr addr) (fromIntegral sz)) $ Unsafe.unsafeCoerce v
    where !(ForeignPtr addr _, sz) = S.unsafeToForeignPtr0 v
  _ -> error "unknown size"

foreign import ccall unsafe "vec_bswap16" c_vec_bswap16 :: Ptr Int8 -> Ptr Int8 -> CSize -> ()
foreign import ccall unsafe "vec_bswap32" c_vec_bswap32 :: Ptr Int8 -> Ptr Int8 -> CSize -> ()
foreign import ccall unsafe "vec_bswap64" c_vec_bswap64 :: Ptr Int8 -> Ptr Int8 -> CSize -> ()

#endif
