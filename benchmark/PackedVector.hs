{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

import Prelude hiding (foldMap)

import Test.Tasty.Bench

import Prelude hiding (length)
import qualified Foreign.Storable as S
import qualified Data.Primitive.Ptr as Ptr

import qualified Data.Foldable
import Data.Semigroup
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Word
import qualified Foreign.ForeignPtr as FP
import Data.Coerce
import Data.Bits
import Control.Monad.Primitive
import Control.Monad.ST ( runST )
import GHC.Base
import GHC.ForeignPtr ( unsafeWithForeignPtr )
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Primitive as Prim

import Util.Vector.Packed hiding (nrWords)

data Env = Env {
    staticV4  :: PackedVector ('Static 4096) ('Static 4)
  , staticV5  :: PackedVector ('Static 4096) ('Static 5)
  , staticV6  :: PackedVector ('Static 4096) ('Static 6)
  , staticV7  :: PackedVector ('Static 4096) ('Static 7)
  , staticV8  :: PackedVector ('Static 4096) ('Static 8)
  , staticV9  :: PackedVector ('Static 4096) ('Static 9)
  , staticV15 :: PackedVector ('Static 4096) ('Static 15)
  }
  deriving stock Generic
  deriving anyclass NFData

mkVector :: Int -> IO (FP.ForeignPtr Word64)
mkVector sz =
  let wLen = nrWords sz 4096
  in FP.mallocForeignPtrArray @Word64 wLen

setupEnv :: IO Env
setupEnv = do
  
  sV4 <- mkVector 4
  sV5 <- mkVector 5
  sV6 <- mkVector 6
  sV7 <- mkVector 7
  sV8 <- mkVector 8
  sV9 <- mkVector 9
  sV15 <- mkVector 15

  let staticV4  = unsafeStaticFromForeignPtr sV4
      staticV5  = unsafeStaticFromForeignPtr sV5
      staticV6  = unsafeStaticFromForeignPtr sV6
      staticV7  = unsafeStaticFromForeignPtr sV7
      staticV8  = unsafeStaticFromForeignPtr sV8
      staticV9  = unsafeStaticFromForeignPtr sV9
      staticV15 = unsafeStaticFromForeignPtr sV15

  return $ Env{..}

main :: IO ()
main = do
  defaultMain [
    bgroup "PackedVector" $
      [ env setupEnv $ \ ~(Env{..}) ->
        bgroup "countElemsNaive1" $
          [ bench "countElemsNaive1 4"  $ nf (naiveCount1 $ Prim.fromList [0,3,5]) staticV4
          , bench "countElemsNaive1 5"  $ nf (naiveCount1 $ Prim.fromList [0,3,5]) staticV5
          , bench "countElemsNaive1 6"  $ nf (naiveCount1 $ Prim.fromList [0,3,5]) staticV6
          , bench "countElemsNaive1 7"  $ nf (naiveCount1 $ Prim.fromList [0,3,5]) staticV7
          , bench "countElemsNaive1 8"  $ nf (naiveCount1 $ Prim.fromList [0,3,5]) staticV8
          , bench "countElemsNaive1 9"  $ nf (naiveCount1 $ Prim.fromList [0,3,5]) staticV9
          , bench "countElemsNaive1 15" $ nf (naiveCount1 $ Prim.fromList [0,3,5]) staticV15
          ]
      , env setupEnv $ \ ~(Env{..}) ->
        bgroup "countElemsNaive2" $
          [ bench "countElemsNaive2 4"  $ nf (naiveCount2 $ Prim.fromList [0,3,5]) staticV4
          , bench "countElemsNaive2 5"  $ nf (naiveCount2 $ Prim.fromList [0,3,5]) staticV5
          , bench "countElemsNaive2 6"  $ nf (naiveCount2 $ Prim.fromList [0,3,5]) staticV6
          , bench "countElemsNaive2 7"  $ nf (naiveCount2 $ Prim.fromList [0,3,5]) staticV7
          , bench "countElemsNaive2 8"  $ nf (naiveCount2 $ Prim.fromList [0,3,5]) staticV8
          , bench "countElemsNaive2 9"  $ nf (naiveCount2 $ Prim.fromList [0,3,5]) staticV9
          , bench "countElemsNaive2 15" $ nf (naiveCount2 $ Prim.fromList [0,3,5]) staticV15
          ]
      , env setupEnv $ \ ~(Env{..}) ->
        bgroup "countElems" $
          [ bench "countElems 4"  $ nf (countElems $ Prim.fromList [0,3,5]) staticV4
          , bench "countElems 5"  $ nf (countElems $ Prim.fromList [0,3,5]) staticV5
          , bench "countElems 6"  $ nf (countElems $ Prim.fromList [0,3,5]) staticV6
          , bench "countElems 7"  $ nf (countElems $ Prim.fromList [0,3,5]) staticV7
          , bench "countElems 8"  $ nf (countElems $ Prim.fromList [0,3,5]) staticV8
          , bench "countElems 9"  $ nf (countElems $ Prim.fromList [0,3,5]) staticV9
          , bench "countElems 15" $ nf (countElems $ Prim.fromList [0,3,5]) staticV15
          ]
      , bgroup "unsafeCopy" $
          [ env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 5 - 4" $ nfIO $ copyArr staticV4 staticV5
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 7 - 4" $ nfIO $ copyArr staticV4 staticV7
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 9 - 4" $ nfIO $ copyArr staticV4 staticV9
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 15 - 4" $ nfIO $ copyArr staticV4 staticV15
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 4 - 5" $ nfIO $ copyArr staticV5 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 4 - 7" $ nfIO $ copyArr staticV7 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 4 - 9" $ nfIO $ copyArr staticV9 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 4 - 15" $ nfIO $ copyArr staticV15 staticV4
          ,  env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 4 - 4" $ nfIO $ copyArr staticV4 staticV4
          ]
        , bgroup "naiveCopy1" $
          [ env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy1 5 - 4" $ nfIO $ naiveCopy1 staticV4 staticV5
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy1 7 - 4" $ nfIO $ naiveCopy1 staticV4 staticV7
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy1 9 - 4" $ nfIO $ naiveCopy1 staticV4 staticV9
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy1 15 - 4" $ nfIO $ naiveCopy1 staticV4 staticV15
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy1 4 - 5" $ nfIO $ naiveCopy1 staticV5 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy1 4 - 7" $ nfIO $ naiveCopy1 staticV7 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy1 4 - 9" $ nfIO $ naiveCopy1 staticV9 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy1 4 - 15" $ nfIO $ naiveCopy1 staticV15 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy1 4 - 4" $ nfIO $ naiveCopy1 staticV4 staticV4
          ]
        , bgroup "naiveCopy2" $
          [ env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy2 5 - 4" $ nfIO $ naiveCopy2 staticV4 staticV5
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy2 7 - 4" $ nfIO $ naiveCopy2 staticV4 staticV7
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy2 9 - 4" $ nfIO $ naiveCopy2 staticV4 staticV9
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy2 15 - 4" $ nfIO $ naiveCopy2 staticV4 staticV15
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy2 4 - 5" $ nfIO $ naiveCopy2 staticV5 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy2 4 - 7" $ nfIO $ naiveCopy2 staticV7 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy2 4 - 9" $ nfIO $ naiveCopy2 staticV9 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy2 4 - 15" $ nfIO $ naiveCopy2 staticV15 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "naiveCopy2 4 - 4" $ nfIO $ naiveCopy2 staticV4 staticV4
          ]
      ]
    ]

naiveCount1 :: PVector v => Prim.Vector Word -> v -> Int
naiveCount1 els = \v -> coerce $ foldMap1 (\x -> if Prim.elem x els then Sum (1 :: Int) else Sum 0) v
{-# INLINE naiveCount1 #-}

naiveCount2 :: PVector v => Prim.Vector Word -> v -> Int
naiveCount2 els = \v -> coerce $ foldMap2 (\x -> if Prim.elem x els then Sum (1 :: Int) else Sum 0) v
{-# INLINE naiveCount2 #-}

foldMap1 :: (PVector v, Monoid m) => (Word -> m) -> v -> m
foldMap1 f v = runST $ do
  mv <- unsafeThaw v
  let len = Util.Vector.Packed.length mv
      bSz = bitSz mv
      fptr = backing mv
      wLen = nrWords bSz len
      perWord = 64 `divInt` bSz
      loop !i !arrI !ptr !b
        | i >= len  = pure b
        | otherwise = (S.peek ptr) >>= (\w -> loopInner i arrI w 0 b) >>= loop (i + perWord) (arrI + 1) (Ptr.advancePtr ptr 1)
      loopInner !i !arrI !w !n !b
        | n >= perWord || i >= len = pure b
        | otherwise                     = loopInner (i + 1) arrI w (n + 1) (f el)
        where
          el = fromIntegral $ ((unsafeShiftL 1 bSz) - 1) .&. (unsafeShiftR w $ i * bSz)
  unsafePrimToPrim $ unsafeWithForeignPtr fptr $ \ptr -> loop (0 :: Int) (0 :: Int) ptr mempty
  where
{-# INLINE foldMap1 #-}

foldMap2 :: (PVector v, Monoid m) => (Word -> m) -> v -> m
foldMap2 f v = runST $ do
  mv <- unsafeThaw v
  let len = Util.Vector.Packed.length mv
      bSz = bitSz mv
      fptr = backing mv
      wLen = nrWords bSz len
      perWord = 64 `divInt` bSz
      go ptr n x acc
        | n < wLen = do
          w <- S.peek ptr
          let nAcc = acc <> (Prelude.foldMap (\i -> f . fromIntegral . (.&. ((unsafeShiftL 1 bSz) - 1)) $ unsafeShiftR w $ i * bSz) [0..((min perWord (len - x))) - 1])
          go (Ptr.advancePtr ptr 1) (n + 1) (x + perWord) nAcc
        | otherwise = pure acc
  unsafePrimToPrim $ unsafeWithForeignPtr fptr $ \ptr -> go ptr (0 :: Int) (0 :: Int) mempty
  where
{-# INLINE foldMap2 #-}

foldl'1 :: PVector v => (a -> Int -> Word -> a) -> a -> v -> a
foldl'1 f a v = runST $ do
  mv <- unsafeThaw v
  let len = Util.Vector.Packed.length mv
      bSz = bitSz mv
      fptr = backing mv
      wLen = nrWords bSz len
      perWord = 64 `divInt` bSz
      loop !i !arrI !ptr !b
        | i >= len  = pure b
        | otherwise = (S.peek ptr) >>= (\w -> loopInner i arrI w 0 b) >>= loop (i + perWord) (arrI + 1) (Ptr.advancePtr ptr 1)
      loopInner !i !arrI !w !n !b
        | n >= perWord || i >= len = pure b
        | otherwise                = loopInner (i + 1) arrI w (n + 1) (f b i el)
        where
          el = fromIntegral $ ((unsafeShiftL 1 bSz) - 1) .&. (unsafeShiftR w $ i * bSz)
  unsafePrimToPrim $ unsafeWithForeignPtr fptr $ \ptr -> loop (0 :: Int) (0 :: Int) ptr a
{-# INLINE foldl'1 #-}

copyArr :: (PVector v, PVector w) => v -> w -> IO ()
copyArr v w = do
  mv <- unsafeThaw v
  mw <- unsafeThaw w
  unsafeCopy mv mw

naiveCopy1 :: (PVector v, PVector w) => v -> w -> IO ()
naiveCopy1 v w = do
  mv <- unsafeThaw v
  foldl' (\acc i x -> acc >> unsafeWrite mv i x) (pure ()) w

naiveCopy2 :: (PVector v, PVector w) => v -> w -> IO ()
naiveCopy2 v w = do
  mv <- unsafeThaw v
  foldl'1 (\acc i x -> acc >> unsafeWrite mv i x) (pure ()) w


--
nrWords :: Int -> Int -> Int
nrWords bSz i = (i + perWord - 1) `div` perWord
  where perWord = 64 `div` bSz
{-# INLINE nrWords #-}
