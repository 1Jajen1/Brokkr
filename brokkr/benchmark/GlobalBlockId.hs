{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -O2 -dsuppress-all -ddump-simpl -ddump-asm #-}
module Main (search, searchNoPref, searchBST, main) where

import Test.Tasty.Bench

import GHC.Exts
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Data.Word
import GHC.Generics
import Control.DeepSeq
import System.Random
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Data.List (sort)
import Control.Monad.ST (runST)
import Control.Monad

data Env = Env {
  arr  :: S.Vector Word64
, arr2 :: S.Vector Word64
}
  deriving stock Generic
  deriving anyclass NFData

setupEnv :: IO Env
setupEnv = do
  let gen = mkStdGen 3
      len = 8192 * 16
      arr = S.fromList . sort $ take len $ randoms gen
      arr2 = runST $ do
        let l = S.length arr
        hsMar <- SM.new (1 + l)
        let eytzinger !i !k = if k <= l
              then do
                i' <- eytzinger i (2 * k)
                SM.unsafeWrite hsMar  k (S.unsafeIndex arr i')
                eytzinger (i' + 1) (2 * k + 1)
              else pure i
        void $ eytzinger 0 1
        S.unsafeFreeze hsMar
  pure Env{..}

main = pure ()

-- TODO Benchmark against c code here
main2 :: IO ()
main2 = defaultMain [
    env setupEnv $ \ ~(Env{..}) ->
    bgroup "Sorted search" $
      [ bench "search" $ nf (search 100##) arr2
      , bench "searchNoPre" $ nf (searchNoPref 100##) arr2
      , bench "searchBST" $ nf (searchBST 100##) arr
      ]
  ]

search :: Word# -> S.Vector Word64 -> Bool
search hs arr = resVal
  where
    (fptr,l) = S.unsafeToForeignPtr0 arr
    !(I# len) = l
    !(Ptr hsLit) = unsafeForeignPtrToPtr fptr
    blockSize = 8# -- 64 / 8 (cacheline / intSize)
    kRes = runRW# (\s -> int2Word# (loop s 1#))
    firstSet = plusWord# 1## (ctz# (not# kRes))
    kInd = word2Int# (shiftRL# kRes (word2Int# firstSet))
    -- This is slightly wrong if the element we search for is 0. This doesn't happen for lookups
    resVal = isTrue# (kInd /=# 0#) && isTrue# (eqWord# hs (word64ToWord# (indexWord64OffAddr# hsLit kInd)))
    loop s0 !k
      | isTrue# (k ># len) = k
      | otherwise =
        let a = word64ToWord# (indexWord64OffAddr# hsLit k)
        in loop (prefetchAddr0# hsLit (k *# blockSize) s0) (2# *# k +# ltWord# a hs)
{-# NOINLINE search #-}

searchNoPref :: Word# -> S.Vector Word64 -> Bool
searchNoPref hs arr = resVal
  where
    (fptr,l) = S.unsafeToForeignPtr0 arr
    !(I# len) = l
    !(Ptr hsLit) = unsafeForeignPtrToPtr fptr
    kRes = int2Word# (loop 1#)
    firstSet = plusWord# 1## (ctz# (not# kRes))
    kInd = word2Int# (shiftRL# kRes (word2Int# firstSet))
    resVal = isTrue# (kInd /=# 0#) && isTrue# (eqWord# hs (word64ToWord# (indexWord64OffAddr# hsLit kInd)))
    loop !k
      | isTrue# (k ># len) = k
      | otherwise =
        let a = word64ToWord# (indexWord64OffAddr# hsLit k)
        in loop (2# *# k +# ltWord# a hs)
{-# NOINLINE searchNoPref #-}

searchBST :: Word# -> S.Vector Word64 -> Bool
searchBST hs arr = loop 0# len
  where
    (fptr,l0) = S.unsafeToForeignPtr0 arr
    !(I# len) = l0
    !(Ptr hsLit) = unsafeForeignPtrToPtr fptr
    loop !l !u
      | isTrue# (l >=# u) = False 
      | otherwise =
        let a = W# (word64ToWord# (indexWord64OffAddr# hsLit mid#))
            hsW = W# hs
        in case compare a hsW of
          LT -> loop (mid# +# 1#) u
          EQ -> True
          GT -> loop l mid#
      where
        mid# = (l +# u) `quotInt#` 2#
{-# NOINLINE searchBST #-}
