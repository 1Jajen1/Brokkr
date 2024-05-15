{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
module ModifiedUtf8Spec (
  testModifiedUtf8
, genModifiedUtf8
) where

import Data.ByteString qualified as BS

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Data.Bits
import Data.Char (ord, chr)

import Brokkr.NBT.NBTString.Internal

data Validator = SIMD | Branchy
  deriving stock (Eq, Show)

testModifiedUtf8 :: TestTree
testModifiedUtf8 = testGroup "modified utf-8" $
  [ testProperty "valid" . H.withTests 1000 . H.property $ do
      (valid,_) <- H.forAll (genModifiedUtf8 (HR.exponential 0 16384) HG.unicode)
      -- Choose SIMD or branchy
      validator <- H.forAll $ HG.choice [pure Branchy, pure SIMD]
      let prefix = case validator of SIMD -> BS.pack $ replicate 64 1; Branchy -> BS.empty
      H.annotateShow $ prefix <> valid
      H.assert $ isValidModifiedUtf8SIMD $ prefix <> valid
  , testProperty "invalid" . H.withTests 1000 . H.property $ do
      (valid,_) <- H.forAll (genModifiedUtf8 (HR.exponential 0 16384) HG.unicode)
      -- Choose SIMD or branchy
      validator <- H.forAll $ HG.choice [pure Branchy, pure SIMD]
      fault <- H.forAll $ genModifiedUtf8Fault
      -- Generate a few ones at the end to catch incomplete patterns
      -- Either is generated right biased so most faults will be in the middle later
      index <- fmap (either id id) . H.forAll $ HG.either (HG.int (HR.constant (max 0 $ BS.length valid - 4) (BS.length valid))) (HG.int (HR.constant 0 (BS.length valid)))
      let prefix = case validator of SIMD -> BS.pack $ replicate 64 1; Branchy -> BS.empty
          invalid = applyFault valid index fault
      H.annotateShow $ prefix <> invalid
      H.assert . not $ isValidModifiedUtf8SIMD $ prefix <> invalid
  ] <>
  [ testCase "devTest" $
      let chars :: String = "9¢鏡,65536😜"
          _bs = BS.pack $ replicate 64 1 <> concatMap encodeChar chars
          bs0 = BS.pack $ replicate 64 1 <> [0xE0, 0xA0]
          text = T.pack chars
          _bs' = TE.encodeUtf8 text
      in do
        -- assert $ isValidModifiedUtf8 bs
        -- e <- try @SomeException $ evaluate bs'
        -- print e
        assertBool "" . not $ isValidModifiedUtf8SIMD bs0
  {- 
    -- download https://archives.haskell.org/projects.haskell.org/text/text-testdata.tar.bz2 and extract to benchmark/data. Then remove the intermediate folders
    -- to run these tests
  , testCase "ascii" $ do
    bs0 <- BS.readFile "benchmark/data/ascii.txt"
    let bs = BS.pack $ concatMap encodeChar $ T.unpack $ TE.decodeUtf8 bs0
    assertBool "Should be valid modified utf8" $ isValidModifiedUtf8SIMD bs
  , testCase "bmp" $ do
    bs0 <- BS.readFile "benchmark/data/bmp.txt"
    let bs = BS.pack $ concatMap encodeChar $ T.unpack $ TE.decodeUtf8 bs0
    assertBool "Should be valid modified utf8" $ isValidModifiedUtf8SIMD bs
  , testCase "korean" $ do
    bs0 <- BS.readFile "benchmark/data/korean.txt"
    let bs = BS.pack $ concatMap encodeChar $ T.unpack $ TE.decodeUtf8 bs0
    assertBool "Should be valid modified utf8" $ isValidModifiedUtf8SIMD bs
  , testCase "yiwiki" $ do
    bs0 <- BS.readFile "benchmark/data/yiwiki.xml"
    let bs = BS.pack $ concatMap encodeChar $ T.unpack $ TE.decodeUtf8 bs0
    assertBool "Should be valid modified utf8" $ isValidModifiedUtf8SIMD bs
  , testCase "chinese" $ do
    bs0 <- BS.readFile "benchmark/data/chinese.txt"
    let bs = BS.pack $ concatMap encodeChar $ T.unpack $ TE.decodeUtf8 bs0
    assertBool "Should be valid modified utf8" $ isValidModifiedUtf8SIMD bs
  , testCase "chinese" $ do
    bs0 <- BS.readFile "benchmark/data/test.txt"
    let bs = BS.pack $ concatMap encodeChar $ T.unpack $ TE.decodeUtf8 bs0
    assertBool "Should be valid modified utf8" $ isValidModifiedUtf8SIMD bs
  , testCase "smp_sip" $ do
    bs0 <- BS.readFile "benchmark/data/smp_sip.txt"
    let bs = BS.pack $ concatMap encodeChar $ T.unpack $ TE.decodeUtf8 bs0
    assertBool "Should be valid modified utf8" $ isValidModifiedUtf8SIMD bs
  , testCase "smp_sip2" $ do
    bs0 <- BS.readFile "benchmark/data/smp_sip2.txt"
    let bs = BS.pack $ concatMap encodeChar $ T.unpack $ TE.decodeUtf8 bs0
    assertBool "Should be valid modified utf8" $ isValidModifiedUtf8SIMD bs
  , testCase "cuneiform" $ do
    bs0 <- BS.readFile "benchmark/data/cuneiform.txt"
    let bs = BS.pack $ concatMap encodeChar $ T.unpack $ TE.decodeUtf8 bs0
    assertBool "Should be valid modified utf8" $ isValidModifiedUtf8SIMD bs
  -}
  ]
  where
    encodeChar c
      | cp == 0    = [ 0xC0, 0x80 ]
      | cp <= 127  = [ fromIntegral cp ]
      | cp <= 2047 = [ 0xC0 .|. fromIntegral (cp `unsafeShiftR` 6)
                     , 0x80 .|. fromIntegral (cp .&. 0x3F)
                     ]
      | cp <= 0xD7FF || (cp >= 0xE000 && cp <= 0xFFFF)
                   = [ 0xE0 .|. fromIntegral (cp `unsafeShiftR` 12)
                     , 0x80 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x3F)
                     , 0x80 .|. fromIntegral (cp .&. 0x3F)
                     ]
      | cp >= 0x10000 && cp <= 0x10FFFF
                   = [ 0xED, 0xA0 .|. fromIntegral ((cp `unsafeShiftR` 16) - 1)   , 0x80 .|. fromIntegral ((cp `unsafeShiftR` 10) .&. 0x3F)
                     , 0xED, 0xB0 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x7), 0x80 .|. fromIntegral (cp .&. 0x3F)
                     ]
      | otherwise     = error $ "invalid unicode code point " <> show cp
      where cp = ord c


genModifiedUtf8 :: HR.Range Int -> H.Gen Char -> H.Gen (BS.ByteString, String)
genModifiedUtf8 range charGen = do
  chars <- HG.list range charGen
  pure (BS.pack $ concatMap encodeChar chars, chars)
  where
    encodeChar c
      | cp == 0    = [ 0xC0, 0x80 ]
      | cp <= 127  = [ fromIntegral cp ]
      | cp <= 2047 = [ 0xC0 .|. fromIntegral (cp `unsafeShiftR` 6)
                     , 0x80 .|. fromIntegral (cp .&. 0x3F)
                     ]
      | cp <= 0xD7FF || (cp >= 0xE000 && cp <= 0xFFFF)
                   = [ 0xE0 .|. fromIntegral (cp `unsafeShiftR` 12)
                     , 0x80 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x3F)
                     , 0x80 .|. fromIntegral (cp .&. 0x3F)
                     ]
      | cp >= 0x10000 && cp <= 0x10FFFF
                   = [ 0xED, 0xA0 .|. fromIntegral ((cp `unsafeShiftR` 16) - 1)   , 0x80 .|. fromIntegral ((cp `unsafeShiftR` 10) .&. 0x3F)
                     , 0xED, 0xB0 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x7), 0x80 .|. fromIntegral (cp .&. 0x3F)
                     ]
      | otherwise     = error $ "invalid unicode code point " <> show cp
      where cp = ord c

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

applyFault :: BS.ByteString -> Int -> ModUtf8Fault -> BS.ByteString
applyFault bs at = \case
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
