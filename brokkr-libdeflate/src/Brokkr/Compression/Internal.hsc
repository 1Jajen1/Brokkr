{-# LANGUAGE DeriveAnyClass #-}
module Brokkr.Compression.Internal (
  Decompressor(..), withDecompressor
, newDecompressor, newDecompressorWith, freeDecompressor
, DeflateException(..), decompressWith
, Compressor(..), withCompressor
, CompressionLevel(..)
, newCompressor, newCompressorWith, freeCompressor
, CompressionException(..), compressWith
, CompressionOptions(..)
) where

import Control.Exception

import Data.Word (Word8)

import Foreign.C.Types (CSize, CInt(..))
import Foreign.Storable

import GHC.Exts

#include "libdeflate.h"

data CompressionOptions = CompressionOptions {
  malloc :: {-# UNPACK #-} !(FunPtr (CSize -> IO (Ptr ())))
, free   :: {-# UNPACK #-} !(FunPtr (Ptr () -> IO ()))
}

instance Storable CompressionOptions where
  sizeOf _ = #{size struct libdeflate_options}
  alignment _ = #{alignment struct libdeflate_options}
  peek p = CompressionOptions
    <$> #{peek struct libdeflate_options, malloc_func} p
    <*> #{peek struct libdeflate_options, free_func}   p
  poke p (CompressionOptions m f) = do
    #{poke struct libdeflate_options, sizeof_options} p (fromIntegral @Int @CSize #{size struct libdeflate_options})
    #{poke struct libdeflate_options, malloc_func}    p m
    #{poke struct libdeflate_options, free_func}      p f

newtype Decompressor = Decompressor (Ptr Decompressor)

withDecompressor :: (Decompressor -> IO a) -> IO a
{-# INLINE withDecompressor #-}
withDecompressor f = bracket newDecompressor freeDecompressor $ f . Decompressor

newDecompressor :: IO (Ptr Decompressor)
newDecompressor = c_libdeflate_alloc_decompressor

newDecompressorWith :: Ptr CompressionOptions -> IO (Ptr Decompressor)
newDecompressorWith = c_libdeflate_alloc_decompressor_ex

freeDecompressor :: Ptr Decompressor -> IO ()
freeDecompressor = c_libdeflate_free_decompressor

decompressWith
  :: (DeflateException -> IO r)
  -> IO r
  -> (Ptr Decompressor -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt)
  -> Ptr Decompressor -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr CSize
  -> IO r
{-# INLINE decompressWith #-}
decompressWith onErr onSucc decompF decompressor inPtr inSz outPtr outSz resSz = do
  ret <- decompF decompressor inPtr (fromIntegral inSz) outPtr (fromIntegral outSz) resSz
  case ret of
    #{const LIBDEFLATE_SUCCESS} -> onSucc
    #{const LIBDEFLATE_BAD_DATA} -> onErr DeflateBadData
    #{const LIBDEFLATE_SHORT_OUTPUT} -> onErr DeflateShortOutput
    #{const LIBDEFLATE_INSUFFICIENT_SPACE} -> onErr DeflateInsufficientSpace
    _ -> onErr (DeflateUnknownRet $ fromIntegral ret)

data DeflateException = DeflateBadData | DeflateShortOutput | DeflateInsufficientSpace | DeflateUnknownRet Int
  deriving stock Show
  deriving anyclass Exception

foreign import ccall unsafe "libdeflate_alloc_decompressor" c_libdeflate_alloc_decompressor :: IO (Ptr Decompressor)
foreign import ccall unsafe "libdeflate_alloc_decompressor_ex" c_libdeflate_alloc_decompressor_ex :: Ptr CompressionOptions -> IO (Ptr Decompressor)
foreign import ccall unsafe "libdeflate_free_decompressor" c_libdeflate_free_decompressor :: Ptr Decompressor -> IO ()

newtype Compressor = Compressor (Ptr Compressor)

newtype CompressionLevel = CompressionLevel Int
  deriving newtype (Eq, Show, Ord, Enum, Num)

withCompressor :: CompressionLevel -> (Compressor -> IO a) -> IO a
{-# INLINE withCompressor #-}
withCompressor lvl f = bracket (newCompressor lvl) freeCompressor $ f . Compressor

newCompressor :: CompressionLevel -> IO (Ptr Compressor)
newCompressor (CompressionLevel lvl) = c_libdeflate_alloc_compressor (fromIntegral lvl)

newCompressorWith :: CompressionLevel -> Ptr CompressionOptions -> IO (Ptr Compressor)
newCompressorWith (CompressionLevel lvl) opts = c_libdeflate_alloc_compressor_ex (fromIntegral lvl) opts

freeCompressor :: Ptr Compressor -> IO ()
freeCompressor = c_libdeflate_free_compressor

compressWith
  :: (CompressionException -> IO r)
  -> (Int -> IO r)
  -> (Ptr Compressor -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO CInt)
  -> Ptr Compressor -> Ptr Word8 -> Int -> Ptr Word8 -> Int
  -> IO r
{-# INLINE compressWith #-}
compressWith onErr onSucc compF compressor inPtr inSz outPtr outSz = do
  ret <- compF compressor inPtr (fromIntegral inSz) outPtr (fromIntegral outSz)
  case ret of
    0 -> onErr CompressionNotEnoughSpace
    _ -> onSucc $ fromIntegral ret

data CompressionException = CompressionNotEnoughSpace
  deriving stock Show
  deriving anyclass Exception

foreign import ccall unsafe "libdeflate_alloc_compressor" c_libdeflate_alloc_compressor :: CInt -> IO (Ptr Compressor)
foreign import ccall unsafe "libdeflate_alloc_compressor_ex" c_libdeflate_alloc_compressor_ex :: CInt -> Ptr CompressionOptions -> IO (Ptr Compressor)
foreign import ccall unsafe "libdeflate_free_compressor" c_libdeflate_free_compressor :: Ptr Compressor -> IO ()
