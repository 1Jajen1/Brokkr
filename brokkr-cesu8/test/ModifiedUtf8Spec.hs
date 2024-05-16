{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
module ModifiedUtf8Spec (
  spec
) where

import Data.ByteString qualified as BS

import Test.Syd
import Test.Syd.Hedgehog ()

import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Data.Bits
import Data.Char (ord, chr)

import Brokkr.ModifiedUtf8 as ModUtf8

data Validator = SIMD | Branchy
  deriving stock (Eq, Show)

numIterations :: Int
numIterations = 1_000

spec :: Spec
spec = describe "modified utf-8" $ do
  modifyMaxSuccess (const numIterations) . it "should accept valid modified utf8" . H.property $ do
      (valid,_) <- H.forAll (genModifiedUtf8 (HR.exponential 0 16384) HG.unicode)
      let validBs = unwrapModifiedUtf8 valid
      -- Choose SIMD or branchy
      validator <- H.forAll $ HG.choice [pure Branchy, pure SIMD]
      let prefix = case validator of SIMD -> BS.pack $ replicate 64 1; Branchy -> BS.empty
      H.annotateShow $ prefix <> validBs
      H.assert $ isValidModifiedUtf8 $ prefix <> validBs
  modifyMaxSuccess (const numIterations) . it "should reject invalid modified utf8" . H.property $ do
      (valid,_) <- H.forAll (genModifiedUtf8 (HR.exponential 0 16384) HG.unicode)
      let validBs = unwrapModifiedUtf8 valid
      -- Choose SIMD or branchy
      validator <- H.forAll $ HG.choice [pure Branchy, pure SIMD]
      fault <- H.forAll $ genModifiedUtf8Fault
      -- Generate a few ones at the end to catch incomplete patterns
      -- Either is generated right biased so most faults will be in the middle later
      index <- fmap (either id id) . H.forAll $ HG.either (HG.int (HR.constant (max 0 $ BS.length validBs - 4) (BS.length validBs))) (HG.int (HR.constant 0 (BS.length validBs)))
      let prefix = case validator of SIMD -> BS.pack $ replicate 64 1; Branchy -> BS.empty
          invalid = applyFault valid index fault
      H.annotateShow $ prefix <> invalid
      H.assert . not $ isValidModifiedUtf8 $ prefix <> invalid
  xit "devTest" $
    let chars :: String = replicate 64 (chr 1) <> "9¢鏡,65536😜"
        bs0 = BS.pack $ [1,0,192,128,237,160,128,237,176,128]
        text = T.pack chars
        _bs' = TE.encodeUtf8 text
    -- assert $ isValidModifiedUtf8 bs
    -- e <- try @SomeException $ evaluate bs'
    -- print e
    in not $ isValidModifiedUtf8 bs0

genModifiedUtf8 :: HR.Range Int -> H.Gen Char -> H.Gen (ModifiedUtf8, String)
genModifiedUtf8 range charGen = do
  chars <- HG.list range charGen
  pure (ModUtf8.pack chars, chars)

genModifiedUtf8Fault :: H.Gen ModUtf8Fault
genModifiedUtf8Fault = HG.choice [
    pure FaultNull
  , HG.enum '\1' '\127' >>= pure . FaultOverlong2
  , HG.enum '\1' '\2047' >>= pure . FaultOverlong3
  , HG.enum '\128' '\2047' >>= pure . FaultTooShort2
  , HG.choice [
      HG.enum '\2047' (chr 0xD7FF)
    , HG.enum (chr 0xE000) (chr 0xFFFF)
    ] >>= \c -> HG.choice [pure $ FaultTooShort3One c, pure $ FaultTooShort3Two c]
  , HG.enum '\1' '\63' >>= pure . FaultLoneContinuation
  , HG.enum (chr 0x10000) (chr 0x10FFFF) >>= \c -> HG.choice [pure $ FaultLoneSurrogateLow c, pure $ FaultLoneSurrogateHigh c, pure $ Fault4ByteEncoding c]
  , fmap chr (HG.enum 0xD800 0xDFFF) >>= pure . FaultInvalidChar
  ]

-- TODO Straight up invalid unicode?

data ModUtf8Fault
  = FaultNull
  | FaultOverlong2 !Char -- Value fits in 7 bits
  | FaultOverlong3 !Char -- Value fits into either 7 or 12 bits
  | FaultTooShort2 !Char -- 2 byte value
  | FaultTooShort3One !Char -- 3 byte value
  | FaultTooShort3Two !Char -- 3 byte value
  | FaultLoneContinuation !Char -- 6 bit value
  | FaultLoneSurrogateLow !Char -- Character in surrogate range
  | FaultLoneSurrogateHigh !Char -- Character in surrogate range
  | Fault4ByteEncoding !Char -- utf8 4 byte encoding
  | FaultInvalidChar !Char -- invalid unicode character
  deriving stock (Eq, Show)

applyFault :: ModifiedUtf8 -> Int -> ModUtf8Fault -> BS.ByteString
applyFault inp at = \case
  FaultNull -> l <> BS.pack [0] <> r
  FaultOverlong2 c -> let cp = ord c in l <> BS.pack [0xC0 .|. fromIntegral (cp `unsafeShiftR` 6), 0x80 .|. fromIntegral (cp .&. 0x3F)] <> r
  FaultOverlong3 c -> let cp = ord c in l <> BS.pack [0xE0 .|. fromIntegral (cp `unsafeShiftR` 12), 0x80 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x3F), 0x80 .|. fromIntegral (cp .&. 0x3F)] <> r
  FaultTooShort2 c -> let cp = ord c in l <> BS.pack [0xC0 .|. fromIntegral (cp `unsafeShiftR` 6)] <> r
  FaultTooShort3One c -> let cp = ord c in l <> BS.pack [0xE0 .|. fromIntegral (cp `unsafeShiftR` 12), 0x80 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x3F)] <> r
  FaultTooShort3Two c -> let cp = ord c in l <> BS.pack [0xE0 .|. fromIntegral (cp `unsafeShiftR` 12)] <> r
  FaultLoneContinuation c -> let cp = ord c in l <> BS.pack [0x80 .|. fromIntegral (cp .&. 0x3F)] <> r
  FaultLoneSurrogateLow c -> let cp = ord c in l <> BS.pack [0xED, 0xA0 .|. fromIntegral ((cp `unsafeShiftR` 16) - 1)   , 0x80 .|. fromIntegral ((cp `unsafeShiftR` 10) .&. 0x3F)] <> r
  FaultLoneSurrogateHigh c -> let cp = ord c in l <> BS.pack [0xED, 0xB0 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x7), 0x80 .|. fromIntegral (cp .&. 0x3F)] <> r
  Fault4ByteEncoding c -> let cp = ord c in l <> BS.pack [0xF0 .|. fromIntegral (cp `unsafeShiftR` 18), 0x80 .|. fromIntegral ((cp `unsafeShiftR` 12) .&. 0x3F), 0x80 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x3F), 0x80 .|. fromIntegral (cp .&. 0x3F)] <> r
  FaultInvalidChar c -> l <> BS.pack (faultyEncodeChar c) <> r
  where
    bs = unwrapModifiedUtf8 inp
    (l, r) = BS.splitAt at bs
    -- All our faulty unicode values are 3 bytes. 4 byte invalid chars get surrogate pairs
    faultyEncodeChar c
      -- Two faulty cases. The valid encode has more bounds here
      | cp > 2047 && cp <= 0xFFFF
                      = [ 0xE0 .|. fromIntegral (cp `unsafeShiftR` 12)
                        , 0x80 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x3F)
                        , 0x80 .|. fromIntegral (cp .&. 0x3F)
                        ]
      | cp >= 0x10000 = [ 0xED, 0xA0 .|. fromIntegral ((cp `unsafeShiftR` 16) - 1)   , 0x80 .|. fromIntegral ((cp `unsafeShiftR` 10) .&. 0x3F)
                        , 0xED, 0xB0 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x7), 0x80 .|. fromIntegral (cp .&. 0x3F)
                        ]
      | otherwise     = error $ "invalid unicode code point " <> show cp
      where cp = ord c
