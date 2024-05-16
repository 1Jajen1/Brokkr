module Main (
  main
) where

import Control.DeepSeq
import Control.Exception

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS

import Test.Tasty.Bench

import Brokkr.Cesu8 as Cesu8
import Brokkr.ModifiedUtf8 as ModUtf8

import Foreign.C.Types
import GHC.Exts
import GHC.Word

-- import Data.Text qualified as T
-- import Data.Text.Encoding qualified as TE

data Variant = Standard | Java

main :: IO ()
main = do
  -- download https://archives.haskell.org/projects.haskell.org/text/text-testdata.tar.bz2 and extract to benchmark/data. Then remove the intermediate folders
  -- and transcode the files to run these tests
  -- TODO Add modified-utf-8 conversion benchmarks
  -- transcodeFile "ascii.txt"
  -- transcodeFile "bmp.txt"
  -- transcodeFile "japanese.txt"
  -- transcodeFile "korean.txt"
  defaultMain [
      bgroup "modified utf8" [
        benchValidate Java "ascii.txt"
      , benchValidate Java "bmp.txt"
      , benchValidate Java "japanese.txt"
      , benchValidate Java "korean.txt"
      ]
    , bgroup "cesu8" [
        benchValidate Standard "ascii.txt"
      , benchValidate Standard "bmp.txt"
      , benchValidate Standard "japanese.txt"
      , benchValidate Standard "korean.txt"
      ]
    ]

-- transcodeFile :: String -> IO ()
-- transcodeFile name = do
--   bs <- BS.readFile ("benchmark/data/" ++ name)
--   let str = T.unpack $ TE.decodeUtf8 bs
--       bsModUtf8 = unwrapModifiedUtf8 $ ModUtf8.pack str
--       bsCesu8 = unwrapCesu8 $ Cesu8.pack str
--   BS.writeFile ("benchmark/data/" ++ name ++ ".modutf8") bsModUtf8
--   BS.writeFile ("benchmark/data/" ++ name ++ ".cesu8") bsCesu8

benchValidate :: Variant -> String -> Benchmark
benchValidate var name = env (BS.readFile ("benchmark/data/" ++ fileName) >>= evaluate . force) $ \ ~bs -> bgroup name
  [ bench "isValid (branchy)" $ nf isValidFB bs
  , bench "isValid (SIMD)" $ nf isValidSIMD bs
  , bench "isValid" $ nf isValid bs
  , bench "isValid (branchy) (64b)" $ nf isValidFB (BS.take 64 bs)
  , bench "isValid (SIMD)  (64b)" $ nf isValidSIMD (BS.take 64 bs)
  , bench "isValid (64b)" $ nf isValid (BS.take 64 bs)
  , bench "isValid (branchy) (32b)" $ nf isValidFB (BS.take 32 bs)
  , bench "isValid (SIMD) (32b)" $ nf isValidSIMD (BS.take 32 bs)
  , bench "isValid (32b)" $ nf isValid (BS.take 32 bs)
  , bench "isValid (branchy) (16b)" $ nf isValidFB (BS.take 16 bs)
  , bench "isValid (SIMD) (16b)" $ nf isValidSIMD (BS.take 16 bs)
  , bench "isValid (16b)" $ nf isValid (BS.take 16 bs)
  , bench "isValid (branchy) (8b)" $ nf isValidFB (BS.take 8 bs)
  , bench "isValid (SIMD) (8b)" $ nf isValidSIMD (BS.take 8 bs)
  , bench "isValid (8b)" $ nf isValid (BS.take 8 bs)
  , bench "isValid (branchy) (4b)" $ nf isValidFB (BS.take 4 bs)
  , bench "isValid (SIMD) (4b)" $ nf isValidSIMD (BS.take 4 bs)
  , bench "isValid (4b)" $ nf isValid (BS.take 4 bs)
  ]
  where
    (isValidFB, isValidSIMD, isValid, fileName) = case var of
      Standard -> (isValidCesu8FB, isValidCesu8SIMD, isValidCesu8, name ++ ".cesu8")
      Java -> (isValidModifiedUtf8FB, isValidModifiedUtf8SIMD, isValidModifiedUtf8, name ++ ".modutf8")

isValidModifiedUtf8FB :: ByteString -> Bool
{-# INLINE isValidModifiedUtf8FB #-}
isValidModifiedUtf8FB (BS.BS fptr len) = BS.accursedUnutterablePerformIO $ BS.unsafeWithForeignPtr fptr $ \ptr -> do
  i <- c_is_valid_modified_utf8_branchy ptr (fromIntegral len)
  pure $ i /= 0

foreign import ccall unsafe "is_valid_modified_utf8_branchy" c_is_valid_modified_utf8_branchy :: Ptr Word8 -> CSize -> IO CInt

isValidCesu8FB :: ByteString -> Bool
{-# INLINE isValidCesu8FB #-}
isValidCesu8FB (BS.BS fptr len) = BS.accursedUnutterablePerformIO $ BS.unsafeWithForeignPtr fptr $ \ptr -> do
  i <- c_is_valid_cesu8_branchy ptr (fromIntegral len)
  pure $ i /= 0

foreign import ccall unsafe "is_valid_cesu8_branchy" c_is_valid_cesu8_branchy :: Ptr Word8 -> CSize -> IO CInt

isValidModifiedUtf8SIMD :: ByteString -> Bool
{-# INLINE isValidModifiedUtf8SIMD #-}
isValidModifiedUtf8SIMD (BS.BS fptr len) = BS.accursedUnutterablePerformIO $ BS.unsafeWithForeignPtr fptr $ \ptr -> do
  i <- c_is_valid_modified_utf8_simd ptr (fromIntegral len)
  pure $ i /= 0

foreign import ccall unsafe "is_valid_modified_utf8_simd" c_is_valid_modified_utf8_simd :: Ptr Word8 -> CSize -> IO CInt

isValidCesu8SIMD :: ByteString -> Bool
{-# INLINE isValidCesu8SIMD #-}
isValidCesu8SIMD (BS.BS fptr len) = BS.accursedUnutterablePerformIO $ BS.unsafeWithForeignPtr fptr $ \ptr -> do
  i <- c_is_valid_cesu8_simd ptr (fromIntegral len)
  pure $ i /= 0

foreign import ccall unsafe "is_valid_cesu8_simd" c_is_valid_cesu8_simd :: Ptr Word8 -> CSize -> IO CInt