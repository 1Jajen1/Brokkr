{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Brokkr.PackedVector.Util (
  nrOfWords
, index
, mask
, foldMap
, divConstants
, mulHi
, fastDiv
, fastMod
) where

import Data.Word
import Data.Bits
import Data.Int
import Foreign.ForeignPtr (ForeignPtr)
import Control.Monad.Primitive (PrimBase, unsafePrimToPrim, unsafePrimToIO)
import qualified Foreign.Storable as S
import GHC.ForeignPtr (unsafeWithForeignPtr)
import Prelude hiding (foldMap)
import Data.Primitive.Ptr (advancePtr)
import Language.Haskell.TH
import Data.Primitive (sizeOf)
import GHC.Exts

-- All the fast div stuff heavily relies on inlining and constant folding on GHC's side, so make sure
-- that changes don't break this, otherwise performance suffers

-- TODO The code relying on inlining fastDiv/fastMod is obsolete if #9786 is merged into ghc

nrOfWords :: Int -> Int -> Int
nrOfWords len d = fastDiv d (len + 63)
{-# INLINE nrOfWords #-}

index :: Int -> Int -> (Int, Int)
index d i =
  let vecIndex  = fastDiv i d
      wordIndex = fastMod vecIndex i d
  in (vecIndex, wordIndex)
{-# INLINE index #-}

fastDiv :: Int -> Int -> Int
fastDiv i d =
  let (m,a,shft) = divConstants d
  in (mulHi i m + a * i) `unsafeShiftR` shft
{-# INLINE[1] fastDiv #-}
-- These rules aren't technically valid, but I only pass positive numbers so it's fine
{-# RULES
"div by 1" forall i . fastDiv i 1 = i
"div by 2" forall i . fastDiv i 2 = i `unsafeShiftR` 1
"div by 4" forall i . fastDiv i 4 = i `unsafeShiftR` 2
"div by 8" forall i . fastDiv i 8 = i `unsafeShiftR` 3
"div by 16" forall i . fastDiv i 16 = i `unsafeShiftR` 4
"div by 32" forall i . fastDiv i 32 = i `unsafeShiftR` 5
"div by 64" forall i . fastDiv i 64 = i `unsafeShiftR` 6
  #-}
  -- No need for more rules as d is in range 1-64
  -- Also this only works because while we are using signed Int, the actual values will be positive

fastMod :: Int -> Int -> Int -> Int
fastMod q i d = i - d * q
{-# INLINE[1] fastMod #-}
-- These rules aren't technically valid, but I only pass positive numbers so it's fine
{-# RULES
"mod by 1" forall q i . fastMod q i 1 = 0
"mod by 2" forall q i . fastMod q i 2 = i .&. 1
"mod by 4" forall q i . fastMod q i 4 = i .&. 3
"mod by 8" forall q i . fastMod q i 8 = i .&. 7
"mod by 16" forall q i . fastMod q i 16 = i .&. 15
"mod by 32" forall q i . fastMod q i 32 = i .&. 31
"mod by 64" forall q i . fastMod q i 64 = i .&. 63
  #-}

mulHi :: Int -> Int -> Int
mulHi (I# a) (I# b) = I# (case timesInt2# a b of (# _, hi, _ #) -> hi)
{-# INLINE mulHi #-}

divConstants :: Int -> (Int, Int, Int)
divConstants d = $(
  -- Read Hackers delight to understand how this all works
  let genMagic :: Integer -> (Integer, Integer)
      genMagic d = (magic, toInteger $ p - wSz)
        where
          wSz = 8 * sizeOf (undefined :: Int)
          ad = abs d
          t = (1 `shiftL` (wSz - 1)) + if d > 0 then 0 else 1
          anc = t - 1 - rem t ad
          go p'
            | twoP > anc * (ad - rem twoP ad) = p'
            | otherwise = go (p' + 1)
            where twoP = 1 `shiftL` p'
          p = go wSz
          am = (twoP + ad - rem twoP ad) `quot` ad
            where twoP = 1 `shiftL` p
          magic = if d > 0 then am else -am

    in caseE [| d |] $ do
      i :: Int <- [1..64]
      let (m',shft') = genMagic (toInteger i)
          (m,shft) = (fromInteger @Int m', fromInteger @Int shft')
          lz = countTrailingZeros i
      pure $ flip (match (litP $ IntegerL $ toInteger i)) [] $ normalB
        if | popCount i == 1 -> [| (0, 1, lz) |]
           | i > 0 && m < 0 -> [| (m,1,shft) |]
           | i < 0 && m > 0 -> [| (m,-1,shft) |]
           | otherwise -> [| (m, 0, shft) |]
  )
{-# INLINE divConstants #-}

mask :: Int -> Word64
mask bSz = (1 `unsafeShiftL` bSz) - 1
{-# INLINE mask #-}

-- TODO Misleading name
foldMap :: forall m b . PrimBase m => Int -> Int -> ForeignPtr Word64 -> b -> (Int -> Int -> Int -> Word64 -> b -> m b) -> m b
foldMap !elemsPerWord !len !fptr !b1 !f = unsafePrimToPrim . unsafeWithForeignPtr fptr $ \ptr -> unsafePrimToIO $ loop 0 0 ptr b1
  where
    loop !i !arrI !ptr !b
      | i >= len  = pure b
      | otherwise = S.peek ptr >>= (\w -> loopInner i arrI w 0 b) >>= loop (i + elemsPerWord) (arrI + 1) (advancePtr ptr 1)
    loopInner !i !arrI !w !n !b
      | n >= elemsPerWord || i >= len = pure b
      | otherwise                     = unsafePrimToPrim (f i arrI n w b) >>= loopInner (i + 1) arrI w (n + 1)
{-# INLINE foldMap #-}
