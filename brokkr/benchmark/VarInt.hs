{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Test.Tasty.Bench

import qualified Data.Vector.Unboxed as U
import Foreign.ForeignPtr
import Data.Word
import GHC.Generics
import Control.DeepSeq
import Data.Primitive.Ptr
import Data.Int
import GHC.Exts
import Data.Bits
import System.Random.MWC as MWC
import GHC.ForeignPtr (unsafeWithForeignPtr)
import Language.Haskell.TH
import qualified Data.Vector.Storable as S
import GHC.Word

-- Some experiments with writing varInts
-- Relevant: https://steinborn.me/posts/performance/how-fast-can-you-write-a-varint/

data Env = Env {
    numbers :: !(U.Vector Int32)
  , buf     :: !(ForeignPtr Word8)
  }
  deriving stock Generic

instance NFData Env where
  rnf !Env{} = ()

setupEnv :: IO Env
setupEnv = do
  g <- MWC.initialize $ U.fromList [1234]
  numbers <- U.replicateM 2048 (MWC.uniformR (0, maxBound) g)
  buf <- mallocForeignPtrBytes 5
  pure Env{..}

main :: IO ()
main = do

  defaultMain [
      bgroup "write" [
        env setupEnv $ \ ~(Env{..}) -> bench "baseline"  $ nfIO (baselineIO numbers buf)
      , env setupEnv $ \ ~(Env{..}) -> bench "darkArts"  $ nfIO (darkArtsIO numbers buf)
      , env setupEnv $ \ ~(Env{..}) -> bench "pdep"  $ nfIO (oneShotPdepIO numbers buf)
      ]
    ]

baselineIO :: U.Vector Int32 -> ForeignPtr Word8 -> IO ()
baselineIO d fptr = unsafeWithForeignPtr fptr $ \ptr ->
  U.forM_ d $ \i -> writeVarNumInternal (fromIntegral i) ptr
{-# INLINE baselineIO #-}

darkArtsIO :: U.Vector Int32 -> ForeignPtr Word8 -> IO ()
darkArtsIO d fptr = unsafeWithForeignPtr fptr $ \ptr ->
  U.forM_ d $ \i -> darkArts (fromIntegral i) ptr
{-# INLINE darkArtsIO #-}

oneShotPdepIO :: U.Vector Int32 -> ForeignPtr Word8 -> IO ()
oneShotPdepIO d fptr = unsafeWithForeignPtr fptr $ \ptr ->
  U.forM_ d $ \i -> oneShotPdep (fromIntegral i) ptr
{-# INLINE oneShotPdepIO #-}

--------------------------------------------------------------
writeVarNumInternal :: Word64 -> Ptr Word8 -> IO (Ptr Word8)
writeVarNumInternal !n !ptr
  | n < 128 = do
      writeOffPtr ptr 0 (fromIntegral n)
      pure $ advancePtr ptr 1
  | otherwise = do
      writeOffPtr ptr 0 . fromIntegral $ setBit (n .&. 127) 7
      writeVarNumInternal (unsafeShiftR n 7) (advancePtr ptr 1)

darkArts :: Word64 -> Ptr Word8 -> IO (Ptr Word8)
darkArts value ptr =
  if ((value .&. (0xFFFFFFFF `unsafeShiftL` 7)) == 0)
    then do
      writeOffPtr @Word8 ptr 0 (fromIntegral value)
      pure (advancePtr ptr 1)
    else
      if ((value .&. (0xFFFFFFFF `unsafeShiftL` 14)) == 0)
        then do
          let w = (((value .&. 0x7F) .|. 0x80) `unsafeShiftL` 8) .|. (value `unsafeShiftR` 7)
          writeOffPtr @Word16 (coerce ptr) 0 (fromIntegral w)
          pure (advancePtr ptr 2)
        else
          if ((value .&. (0xFFFFFFFF `unsafeShiftL` 21)) == 0)
            then do
              let w1 = value `unsafeShiftR` 14
                  w2 = (((value `unsafeShiftR` 7) .&. 0x7F) .|. 0x80) `unsafeShiftL` 8
                  w3 = ((value .&. 0x7F) .|. 0x80) `unsafeShiftL` 16
                  w = w3 .|. w2 .|. w1
              writeOffPtr @Word16 (coerce ptr) 0 (fromIntegral w)
              writeOffPtr @Word8 ptr 2 (fromIntegral $ w `unsafeShiftR` 16)
              pure (advancePtr ptr 3)
            else
              if ((value .&. (0xFFFFFFFF `unsafeShiftL` 28)) == 0)
                then do
                  let w1 = value `unsafeShiftR` 21
                      w2 = (((value `unsafeShiftR` 14) .&. 0x7F) .|. 0x80) `unsafeShiftL` 8
                      w3 = (((value `unsafeShiftR` 7) .&. 0x7F) .|. 0x80) `unsafeShiftL` 16
                      w4 = ((value .&. 0x7F) .|. 0x80) `unsafeShiftL` 24
                      w = w4 .|. w3 .|. w2 .|. w1
                  writeOffPtr @Word32 (coerce ptr) 0 (fromIntegral w)
                  pure (advancePtr ptr 4)
                else do
                  let w1 = value `unsafeShiftR` 21
                      w2 = (((value `unsafeShiftR` 14) .&. 0x7F) .|. 0x80) `unsafeShiftL` 16
                      w3 = (((value `unsafeShiftR` 7) .&. 0x7F) .|. 0x80) `unsafeShiftL` 124
                      w4 = ((value .&. 0x7F) .|. 0x80) `unsafeShiftL` 24
                      w = w4 .|. w3 .|. w2 .|. w1
                  writeOffPtr @Word32 (coerce ptr) 0 (fromIntegral w)
                  writeOffPtr @Word8 (coerce ptr) 4 (fromIntegral $ w `unsafeShiftR` 28)
                  pure (advancePtr ptr 5)
{-# INLINE darkArts #-}

oneShotPdep :: Word64 -> Ptr Word8 -> IO (Ptr Word8)
oneShotPdep value@(W64# value#) ptr =
  let sz = varIntSize $ fromIntegral value
      !mask = writeMask sz
      !(W# mask#) = complement mask
      !spread = W# (pdep# value# mask#)
  in do
    writeOffPtr (coerce ptr) 0 (spread .|. mask)
    pure (advancePtr ptr sz)
{-# INLINE oneShotPdep #-}

-- TODO Bench against repeated shift and mask if statements
writeMask :: Int -> Word
writeMask (I# n) = W# (indexWord64OffAddr# arr# n)
  where
    arr# = $(
      let (bs, _, _) = S.unsafeToForeignPtr (S.fromList [0x80, 0x8080, 0x808080, 0x80808080, 0x8080808080]) in litE $ bytesPrimL $ mkBytes (coerce @(ForeignPtr Word64) bs) 0 40
      )
{-# INLINE writeMask #-}

varIntSize :: Int -> Int
varIntSize x = lookupByteN arr# (countLeadingZeros $ fromIntegral @_ @Int32 x)
  where arr# = "\ENQ\ENQ\ENQ\ENQ\EOT\EOT\EOT\EOT\EOT\EOT\EOT\ETX\ETX\ETX\ETX\ETX\ETX\ETX\STX\STX\STX\STX\STX\STX\STX\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH"#
{-# INLINE varIntSize #-}

lookupByteN :: Addr# -> Int -> Int
lookupByteN addr# (I# n) = I# (word2Int# word#)
  where word# = word8ToWord# (indexWord8OffAddr# addr# n)
{-# INLINE lookupByteN #-}
  
