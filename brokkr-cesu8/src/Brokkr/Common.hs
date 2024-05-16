{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiWayIf #-}
module Brokkr.Common (
  Variant(..)
, pack
, unpack
) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BS
import Data.Char (ord, chr)

import GHC.Exts
import GHC.ForeignPtr
import GHC.IO
import GHC.ST
import GHC.Word

data Variant = Standard | Java

pack :: Variant -> String -> ByteString
{-# NOINLINE[0] pack #-}
pack _ [] = mempty
pack !var xs0 = runST $ do
  let initLen = 64
  unsafeIOToST $ do
    fp <- mallocPlainForeignPtrBytes initLen
    goPackOuter fp initLen 0 xs0
  where
    goPackOuter !fp !len !off0 xs = unsafeWithForeignPtr fp $ \ptr -> goPackInner ptr off0 xs
      where
        goPackInner :: Ptr Word8 -> Int -> String -> IO ByteString
        goPackInner !_   !off [] = pure $ BS.BS fp off
        goPackInner !ptr@(Ptr addr) !off@(I# off#) ccs@(c:cs)
          | off + 6 > len = do
            let newLen = len * 2
            fp' <- mallocPlainForeignPtrBytes newLen
            unsafeWithForeignPtr fp' $ \(Ptr addr') ->
              IO $ \s -> (# copyAddrToAddr# addr addr' off# s, () #)
            goPackOuter fp' newLen off ccs
          | otherwise = do
            w <- writeCesu8Char (writeWordPtr8 ptr) var off c
            goPackInner ptr (off + w) cs

writeCesu8Char :: (Int -> Word8 -> IO ()) -> Variant -> Int -> Char -> IO Int
{-# INLINE writeCesu8Char #-}
writeCesu8Char writeW8 !var !off c
  | cp == 0, Java <- var = do
    writeW8  off      TWO_BYTE
    writeW8 (off + 1) CONT
    pure 2
  | cp < 128 = do
    writeW8 off $ fromIntegral cp
    pure 1
  | cp < 2048 = do
    writeW8  off      $ TWO_BYTE .|. fromIntegral (cp `unsafeShiftR` 6)
    writeW8 (off + 1) $ CONT .|. fromIntegral (cp .&. 0x3F)
    pure 2
  | cp <= 0xD7FF || (cp >= 0xE000 && cp <= 0xFFFF) = do
    writeW8  off      $ THREE_BYTE .|. fromIntegral (cp `unsafeShiftR` 12)
    writeW8 (off + 1) $ CONT .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x3F)
    writeW8 (off + 2) $ CONT .|. fromIntegral (cp .&. 0x3F)
    pure 3
  | otherwise = do
    writeW8  off      $ SURROGATE
    writeW8 (off + 1) $ SURROGATE_LOW  .|. fromIntegral ((cp `unsafeShiftR` 16) - 1)
    writeW8 (off + 2) $ CONT           .|. fromIntegral ((cp `unsafeShiftR` 10) .&. 0x3F)

    writeW8 (off + 3) $ SURROGATE
    writeW8 (off + 4) $ SURROGATE_HIGH .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x0F)
    writeW8 (off + 5) $ CONT           .|. fromIntegral (cp .&. 0x3F)
    pure 6
  where cp = ord $ safe c

-- From text. Remap surrogate Chars
safe :: Char -> Char
{-# INLINE [0] safe #-}
safe c
  | ord c .&. 0x1ff800 /= 0xd800 = c
  | otherwise                    = '\xfffd'

writeWordPtr8 :: Ptr Word8 -> Int -> Word8 -> IO ()
{-# INLINE writeWordPtr8 #-}
writeWordPtr8 (Ptr addr) (I# off) (W8# w8) = IO $ \s -> (# writeWord8OffAddr# addr off w8 s, () #)

-- TODO Fusion rules to build this from embedded strings?

pattern TWO_BYTE, THREE_BYTE, SURROGATE, SURROGATE_LOW, SURROGATE_HIGH, CONT :: Word8
pattern TWO_BYTE       = 0xC0
pattern THREE_BYTE     = 0xE0
pattern SURROGATE      = 0xED
pattern SURROGATE_LOW  = 0xA0
pattern SURROGATE_HIGH = 0xB0
pattern CONT           = 0x80

-- Note: Assumes a valid encoding and does not check at any point.
-- Note: java vs cesu8 doesn't matter since overlong null is still decoded to null. 
unpack :: ByteString -> String
{-# INLINE unpack #-}
unpack (BS.BS fp sz) = build $ \f z ->
  let goUnpack !ptr !n
        | n >= sz = z
        | True =
          let byte1 = indexByte ptr $ n
              byte2 = indexByte ptr $ n + 1
              byte3 = indexByte ptr $ n + 2
              byte5 = indexByte ptr $ n + 4
              byte6 = indexByte ptr $ n + 5
          in if
            -- Ascii
            | byte1 < 128
              -> f (chr (fromIntegral byte1)) $ goUnpack ptr (n + 1)
            -- Two byte encodings
            | byte1 < THREE_BYTE
              -> f (chr $
              ((fromIntegral $ byte1 .&. 0x1F) `unsafeShiftL` 6) .|.
              ((fromIntegral $ byte2 .&. 0x3F))) $ goUnpack ptr (n + 2)
            -- Three byte surrogate encodings
            | byte1 == SURROGATE && byte2 >= SURROGATE_LOW && byte2 < SURROGATE_HIGH
              -> f (chr $
                  (((fromIntegral $ byte2 .&. 0x0F) + 1) `unsafeShiftL` 16) .|.
                  ((fromIntegral $ byte3 .&. 0x3F) `unsafeShiftL` 10) .|.
                  ((fromIntegral $ byte5 .&. 0x0F) `unsafeShiftL` 6) .|.
                  (fromIntegral $ byte6 .&. 0x3F)) $ goUnpack ptr (n + 6)
            -- Everything else has to be 3 bytes
            | otherwise
              -> f (chr $
                ((fromIntegral $ byte1 .&. 0x0F) `unsafeShiftL` 12) .|.
                ((fromIntegral $ byte2 .&. 0x3F) `unsafeShiftL` 6) .|.
                ((fromIntegral $ byte3 .&. 0x3F))
                ) $ goUnpack ptr (n + 3)
  in BS.accursedUnutterablePerformIO $ unsafeWithForeignPtr fp $ \ptr -> pure $ goUnpack ptr 0

indexByte :: Ptr Word8 -> Int -> Word8
{-# INLINE indexByte #-}
indexByte (Ptr addr) (I# off) = W8# (indexWord8OffAddr# addr off)
