{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
-- {-# OPTIONS_GHC -ddump-cmm -ddump-simpl -dsuppress-all -fforce-recomp #-}
module Main (main, withVarInt) where

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
import Data.ByteString (ByteString)
import Mason.Builder qualified as Mason
import Data.ByteString.Builder.Prim.Internal qualified as Prim
import FlatParse.Basic qualified as Flatparse
import GHC.TypeLits
import Data.Proxy
import Control.Monad

data Env = Env {
    numbers :: !(U.Vector Int32)
  , numbersEnc :: !ByteString
  , buf     :: !(ForeignPtr Word8)
  }
  deriving stock Generic

instance NFData Env where
  rnf Env{} = ()

numEntries :: Int
numEntries = 2048

setupEnv :: IO Env
setupEnv = do
  g <- MWC.initialize $ U.fromList [1234]
  numbers <- U.replicateM numEntries (MWC.uniformR (minBound, maxBound) g)
  let numbersEnc = Mason.toStrictByteString (U.foldMap (\a -> unsafePutVarInt $ fromIntegral a) numbers) 
  buf <- mallocForeignPtrBytes 8
  pure Env{..}

unsafePutVarInt :: Word -> Mason.Builder
{-# INLINE unsafePutVarInt #-}
unsafePutVarInt i = Mason.primBounded varIntPrim i
  where varIntPrim = Prim.boundedPrim 8 pdepEncode

main :: IO ()
main = do

  defaultMain [
      bgroup "write" [
        env setupEnv $ \ ~(Env{..}) -> bench "simpleLoop"  $ nfIO (simpleLoopIO numbers buf)
      , env setupEnv $ \ ~(Env{..}) -> bench "branch size" $ nfIO (branchSizeIO numbers buf)
      , env setupEnv $ \ ~(Env{..}) -> bench "pdep"        $ nfIO (pdepEncodeIO numbers buf)
      ]
    , bgroup "read" [
        env setupEnv $ \ ~(Env{..}) -> bench "simpleLoop" $ nf (decodeSimpleLoop (withVarInt @5 (\_ -> pure ()))) numbersEnc
      , env setupEnv $ \ ~(Env{..}) -> bench "pext"       $ nf (decodeSimpleLoop (pextDecode (\_ -> pure ()))) numbersEnc
      , env setupEnv $ \ ~(Env{..}) -> bench "unrolled"   $ nf (decodeSimpleLoop (unrolledDecode (\_ -> pure ()))) numbersEnc
      ]
    ]

--------------------------------------------------------------
---- Write                                                ----
--------------------------------------------------------------
simpleLoopIO :: U.Vector Int32 -> ForeignPtr Word8 -> IO ()
{-# INLINE simpleLoopIO #-}
simpleLoopIO d fptr = unsafeWithForeignPtr fptr $ \ptr ->
  U.forM_ d $ \i -> writeVarNumInternal (fromIntegral i) ptr

branchSizeIO :: U.Vector Int32 -> ForeignPtr Word8 -> IO ()
{-# INLINE branchSizeIO #-}
branchSizeIO d fptr = unsafeWithForeignPtr fptr $ \ptr ->
  U.forM_ d $ \i -> branchSize (fromIntegral i) ptr

pdepEncodeIO :: U.Vector Int32 -> ForeignPtr Word8 -> IO ()
{-# INLINE pdepEncodeIO #-}
pdepEncodeIO d fptr = unsafeWithForeignPtr fptr $ \ptr ->
  U.forM_ d $ \i -> pdepEncode (fromIntegral i) ptr

--------------------------------------------------------------
---- Write impl                                           ----
--------------------------------------------------------------
writeVarNumInternal :: Word64 -> Ptr Word8 -> IO (Ptr Word8)
writeVarNumInternal !n !ptr
  | n < 128 = do
      writeOffPtr ptr 0 (fromIntegral n)
      pure $ advancePtr ptr 1
  | otherwise = do
      writeOffPtr ptr 0 . fromIntegral $ setBit (n .&. 127) 7
      writeVarNumInternal (unsafeShiftR n 7) (advancePtr ptr 1)

branchSize :: Word64 -> Ptr Word8 -> IO (Ptr Word8)
#ifdef WORDS_BIGENDIAN
branchSize !value !ptr
  | (value .&. (0xFFFFFFFF `unsafeShiftL` 7)) == 0 = do
      writeOffPtr @Word8 ptr 0 (fromIntegral value)
      pure (advancePtr ptr 1)
  | (value .&. (0xFFFFFFFF `unsafeShiftL` 14)) == 0 = do
      let w = (((value .&. 0x7F) .|. 0x80) `unsafeShiftL` 8) .|. (value `unsafeShiftR` 7)
      writeOffPtr @Word16 (coerce ptr) 0 (fromIntegral w)
      pure (advancePtr ptr 2)
  | (value .&. (0xFFFFFFFF `unsafeShiftL` 21)) == 0 = do
      let w1 =    value `unsafeShiftR` 14
          w2 = (((value `unsafeShiftR` 7 ) .&. 0x7F) .|. 0x80) `unsafeShiftL` 8
          w3 = (( value                    .&. 0x7F) .|. 0x80) `unsafeShiftL` 16
          w = w3 .|. w2 .|. w1
      writeOffPtr @Word16 (coerce ptr) 0 (fromIntegral w)
      writeOffPtr @Word8 ptr 2 (fromIntegral $ w `unsafeShiftR` 16)
      pure (advancePtr ptr 3)
   | (value .&. (0xFFFFFFFF `unsafeShiftL` 28)) == 0 = do
      let w1 =    value `unsafeShiftR` 21
          w2 = (((value `unsafeShiftR` 14) .&. 0x7F) .|. 0x80) `unsafeShiftL` 8
          w3 = (((value `unsafeShiftR` 7 ) .&. 0x7F) .|. 0x80) `unsafeShiftL` 16
          w4 = (( value                    .&. 0x7F) .|. 0x80) `unsafeShiftL` 24
          w = w4 .|. w3 .|. w2 .|. w1
      writeOffPtr @Word32 (coerce ptr) 0 (fromIntegral w)
      pure (advancePtr ptr 4)
    | otherwise = do
      let w1 = (( value `unsafeShiftR` 21) .&. 0x7F) .|. 0x80
          w2 = (((value `unsafeShiftR` 14) .&. 0x7F) .|. 0x80) `unsafeShiftL` 8
          w3 = (((value `unsafeShiftR` 7 ) .&. 0x7F) .|. 0x80) `unsafeShiftL` 16
          w4 = (( value                    .&. 0x7F) .|. 0x80) `unsafeShiftL` 24
          w5 = value `unsafeShiftR` 28
          w = w4 .|. w3 .|. w2 .|. w1
      -- trace ("VarInt: (val, w, w1, w2, w3, w4, w5)" <> show (value, w, w1, w2, w3, w4, w5)) pure ()
      writeOffPtr @Word32 (coerce ptr) 0 (fromIntegral w)
      writeOffPtr @Word8 (coerce ptr) 4 (fromIntegral w5)
      pure (advancePtr ptr 5)
#else
branchSize !value !ptr
  | (value .&. (0xFFFFFFFF `unsafeShiftL` 7)) == 0 = do
      writeOffPtr @Word8 ptr 0 (fromIntegral value)
      pure (advancePtr ptr 1)
  | (value .&. (0xFFFFFFFF `unsafeShiftL` 14)) == 0 = do
      let w = ((value .&. 0x7F) .|. 0x80) .|. ((value `unsafeShiftR` 7) `unsafeShiftL` 8)
      writeOffPtr @Word16 (coerce ptr) 0 (fromIntegral w)
      pure (advancePtr ptr 2)
  | (value .&. (0xFFFFFFFF `unsafeShiftL` 21)) == 0 = do
      let w1 = value `unsafeShiftR` 14
          w2 = (((value `unsafeShiftR` 7 ) .&. 0x7F) .|. 0x80) `unsafeShiftL` 8
          w3 = (  value                    .&. 0x7F) .|. 0x80
          w = w3 .|. w2
      writeOffPtr @Word16 (coerce ptr) 0 (fromIntegral w)
      writeOffPtr @Word8 ptr 2 (fromIntegral w1)
      pure (advancePtr ptr 3)
   | (value .&. (0xFFFFFFFF `unsafeShiftL` 28)) == 0 = do
      let w1 = (  value `unsafeShiftR` 21)                     `unsafeShiftL` 24
          w2 = (((value `unsafeShiftR` 14) .&. 0x7F) .|. 0x80) `unsafeShiftL` 16
          w3 = (((value `unsafeShiftR` 7 ) .&. 0x7F) .|. 0x80) `unsafeShiftL` 8
          w4 = (  value                    .&. 0x7F) .|. 0x80
          w = w4 .|. w3 .|. w2 .|. w1
      writeOffPtr @Word32 (coerce ptr) 0 (fromIntegral w)
      pure (advancePtr ptr 4)
    | otherwise = do
      let w1 = (((value `unsafeShiftR` 21) .&. 0x7F) .|. 0x80) `unsafeShiftL` 24
          w2 = (((value `unsafeShiftR` 14) .&. 0x7F) .|. 0x80) `unsafeShiftL` 16
          w3 = (((value `unsafeShiftR` 7 ) .&. 0x7F) .|. 0x80) `unsafeShiftL` 8
          w4 = (  value                    .&. 0x7F) .|. 0x80
          w5 = value `unsafeShiftR` 28
          w = w4 .|. w3 .|. w2 .|. w1
      writeOffPtr @Word32 (coerce ptr) 0 (fromIntegral w)
      writeOffPtr @Word8 (coerce ptr) 4 (fromIntegral w5)
      pure (advancePtr ptr 5)
#endif

pdepEncode :: Word -> Ptr Word8 -> IO (Ptr Word8)
pdepEncode !value !ptr = do
  let !s1 = toS1 value
      !leading = countLeadingZeros s1
      !unusedBs = (leading - 1) `unsafeShiftR` 3
      !bytesNeeded = 8 - unusedBs
      !msbs = 0x8080808080808080
      !msbmask = 0xFFFFFFFFFFFFFFFF `unsafeShiftR` ((8 - bytesNeeded + 1) * 8 - 1)
      !merged = s1 .|. (msbs .&. msbmask)
  writeOffPtr (coerce ptr) 0 merged
  pure (advancePtr ptr bytesNeeded)

toS1 :: Word -> Word
toS1 !w =
        (w .&. 0x000000000000007f)
   .|. ((w .&. 0x0000000000003f80) `unsafeShiftL` 1)
   .|. ((w .&. 0x00000000001fc000) `unsafeShiftL` 2)
   .|. ((w .&. 0x000000000fe00000) `unsafeShiftL` 3)
   .|. ((w .&. 0x00000000f0000000) `unsafeShiftL` 4)
-- TODO I use zen4. pdep should not be as slow as it is?
--  Figure out why... Like  wtf. pext is fine. pdep turns a
-- 14 microseconds bench into 51 microseconds
-- GHC Also generates absolutely awful duplicate code, but that
-- is likely another problem...
-- toS1 (W# value#) = W# (pdep# value# 0x0000000f7f7f7f7f##)


--------------------------------------------------------------
---- Read                                                 ----
--------------------------------------------------------------
decodeSimpleLoop :: Flatparse.Parser e a -> ByteString -> ()
{-# INLINE decodeSimpleLoop #-}
decodeSimpleLoop p bs0 = go 0 bs0
  where
    go !n !bs
      | n >= numEntries = ()
      | otherwise = case Flatparse.runParser p bs of
                      Flatparse.OK !_ !remBs -> go (n + 1) remBs
                      _ -> error $ "Failed parse at " <> show n 

withVarInt :: forall maxSize a e st . KnownNat maxSize => (Int -> Flatparse.ParserT st e a) -> Flatparse.ParserT st e a
{-# INLINE withVarInt #-}
withVarInt f = go 0 0
  where
    maxSz = fromIntegral $ natVal (Proxy @maxSize)
    go !acc !res = do
      b <- Flatparse.anyWord8
      let val    = b .&. 0b01111111
          newRes = res .|. (fromIntegral val `unsafeShiftL` acc)
          newAcc = acc + 7
      when (newAcc > maxSz * 7) Flatparse.empty
      case b .&. 0b10000000 of
        0 -> f newRes
        _ -> go newAcc newRes

pextDecode :: (Int -> Flatparse.ParserT st e a) -> Flatparse.ParserT st e a
{-# INLINE pextDecode #-}
pextDecode f = Flatparse.ParserT $ \fp eob s st ->
    let remSz = minusAddr# eob s
    in if isTrue# (8# ># remSz)
      -- Just go to the loop if this is small...
      -- It is possible to use the other code here as well, but that
      -- requires some way to read n bytes of an addr into a register
      -- and haskell -> cmm cannot do that nicely.
      -- It would need to be asm or c which has ffi overhead
      then case withVarInt @5 f of Flatparse.ParserT g -> g fp eob s st
      else
        let w = indexWordOffAddr# s 0#
            msbs = not# w `and#` not# 0x7f7f7f7f7f7f7f7f##
            len = plusWord# 1## (ctz# msbs)
            varintPart = w `and#` (msbs `xor#` minusWord# msbs 1##)
            res = fromS1 varintPart
            bytesConsumed = len `uncheckedShiftRL#` 3#
        in if isTrue# (5## `ltWord#` bytesConsumed)
          then Flatparse.Fail# st
          else case f (I# (word2Int# res)) of
            Flatparse.ParserT g -> g fp eob (plusAddr# s (word2Int# bytesConsumed)) st

fromS1 :: Word# -> Word#
{-# INLINE fromS1 #-}
-- pext is fine? Why the fuck is pdep not?
fromS1 w = pext# w 0x0000000f7f7f7f7f##
-- fromS1 w =
--         (w `and#` 0x000000000000007f##)
--   `or#` ((w `and#` 0x0000000f00000000##) `uncheckedShiftRL#` 4#)
--   `or#` ((w `and#` 0x000000007f000000##) `uncheckedShiftRL#` 3#)
--   `or#` ((w `and#` 0x00000000007f0000##) `uncheckedShiftRL#` 2#)
--   `or#` ((w `and#` 0x0000000000007f00##) `uncheckedShiftRL#` 1#)

unrolledDecode :: (Int -> Flatparse.ParserT st e a) -> Flatparse.ParserT st e a
{-# INLINE unrolledDecode #-}
unrolledDecode f = Flatparse.ParserT $ \fp eob s st ->
  let remSz = minusAddr# eob s
  in if isTrue# (5# ># remSz)
    -- Just go to the loop if this is small...
    then case withVarInt @5 f of Flatparse.ParserT g -> g fp eob s st
    else
      let w1 = word8ToWord# (indexWord8OffAddr# s 0#)
          done w = isTrue# (w `ltWord#` 0x80##)
          exit res consumed = case f (I# (word2Int# res)) of
                                Flatparse.ParserT g -> g fp eob (plusAddr# s consumed) st
      in if done w1
        then exit w1 1#
        else
          let w2 = word8ToWord# (indexWord8OffAddr# s 1#)
              w2Exit = ((w1 `and#` 0x7F##) `or#` (w2 `uncheckedShiftL#` 7#))
          in if done w2
            then exit w2Exit 2#
            else
              let w3 = word8ToWord# (indexWord8OffAddr# s 2#)
                  w3Exit = w2Exit `or#` (w3 `uncheckedShiftL#` 14#)
              in if done w3
                then exit w3Exit 3#
                else
                  let w4 = word8ToWord# (indexWord8OffAddr# s 3#)
                      w4Exit = w3Exit `or#` (w4 `uncheckedShiftL#` 21#)
                  in if done w4
                    then exit w4Exit 4#
                    else
                      let w5 = word8ToWord# (indexWord8OffAddr# s 4#)
                          w5Exit = w4Exit `or#` (w4 `uncheckedShiftL#` 28#)
                      in if isTrue# (w5 `ltWord#` 16##)
                        then exit w5Exit 5#
                        else Flatparse.Fail# st

