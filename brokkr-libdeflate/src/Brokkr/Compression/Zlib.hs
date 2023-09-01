{-# LANGUAGE MagicHash #-}
module Brokkr.Compression.Zlib (
  decompress, decompress_
, compress, compressBound
, Decompressor, Compressor, CompressionLevel(..)
, withDecompressor, withCompressor
) where

import Brokkr.Compression.Internal

import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BS

import Data.Word

import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

import GHC.Exts (Ptr(..), nullAddr#)
import GHC.ForeignPtr
import GHC.IO (unsafePerformIO)

decompress :: Decompressor -> Int -> ByteString -> Either DeflateException ByteString
decompress (Decompressor decomp) outSz (BS.BS srcFptr compressedSz) = unsafePerformIO $ do
  dstFptr <- BS.mallocByteString outSz
  unsafeWithForeignPtr srcFptr $ \srcPtr ->
    unsafeWithForeignPtr dstFptr $ \dstPtr ->
      decompressWith
        (pure . Left)
        (pure . Right $ BS.BS dstFptr outSz)
        c_libdeflate_zlib_decompress
        decomp srcPtr compressedSz dstPtr outSz (Ptr nullAddr#)

decompress_ :: Decompressor -> Int -> ByteString -> Either DeflateException ByteString
decompress_ (Decompressor decomp) outSz (BS.BS srcFptr compressedSz) = unsafePerformIO $ do
  dstFptr <- BS.mallocByteString outSz
  unsafeWithForeignPtr srcFptr $ \srcPtr ->
    unsafeWithForeignPtr dstFptr $ \dstPtr ->
      alloca $ \resSz ->
        decompressWith
          (pure . Left)
          (peek resSz >>= \sz -> pure . Right $ BS.BS dstFptr (fromIntegral sz))
          c_libdeflate_zlib_decompress
          decomp srcPtr compressedSz dstPtr outSz resSz

foreign import ccall "libdeflate_zlib_decompress" c_libdeflate_zlib_decompress
  :: Ptr Decompressor -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt

compress :: Compressor -> Int -> ByteString -> Maybe ByteString
compress (Compressor comp) outSz (BS.BS srcFptr srcSz) = unsafePerformIO $ do
  dstFptr <- BS.mallocByteString outSz
  unsafeWithForeignPtr srcFptr $ \srcPtr ->
    unsafeWithForeignPtr dstFptr $ \dstPtr ->
      compressWith
        (\_ -> pure Nothing)
        (\sz -> pure . Just $ BS.BS dstFptr sz)
        c_libdeflate_deflate_compress
        comp srcPtr srcSz dstPtr outSz

foreign import ccall "libdeflate_deflate_compress" c_libdeflate_deflate_compress
  :: Ptr Compressor -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO CInt

compressBound :: Compressor -> Int -> Int
compressBound (Compressor comp) inSz =
  fromIntegral $ c_libdeflate_deflate_compress_bound comp (fromIntegral inSz)

foreign import ccall "libdeflate_deflate_compress_bound" c_libdeflate_deflate_compress_bound
  :: Ptr Compressor -> CSize -> CSize
