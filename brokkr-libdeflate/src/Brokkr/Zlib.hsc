{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Brokkr.Zlib (
  decompressIO
) where

import Control.Exception

import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BS

import Data.Word

import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

import GHC.ForeignPtr
import GHC.Ptr

#include "libdeflate.h"

data Compressor

decompressIO :: Int -> ByteString -> IO ByteString
decompressIO decompressedSz (BS.BS srcFPtr compressedSz) = do
  dstFPtr <- BS.mallocByteString decompressedSz
  unsafeWithForeignPtr srcFPtr $ \srcPtr ->
    unsafeWithForeignPtr dstFPtr $ \dstPtr ->
      alloca $ \dstSzPtr -> do
        bracket
          c_libdeflate_alloc_decompressor
          c_libdeflate_free_decompressor
          $ \comp ->
            c_libdeflate_zlib_decompress comp srcPtr (fromIntegral compressedSz) dstPtr (fromIntegral decompressedSz) dstSzPtr >>= \case
              #{const LIBDEFLATE_SUCCESS} -> do
                resSz <- fromIntegral <$> peek dstSzPtr
                pure $ BS.BS dstFPtr resSz
              #{const LIBDEFLATE_BAD_DATA} -> do
                throwIO BadData
              #{const LIBDEFLATE_INSUFFICIENT_SPACE} -> do
                throwIO NotEnoughSpace
              r -> throwIO $ UnknownError (fromIntegral r)
{-# INLINE decompressIO #-}

data DeflateError = BadData | NotEnoughSpace | UnknownError Int
  deriving stock Show
  deriving anyclass Exception

foreign import ccall unsafe "libdeflate_alloc_decompressor" c_libdeflate_alloc_decompressor :: IO (Ptr Compressor)
foreign import ccall unsafe "libdeflate_free_decompressor" c_libdeflate_free_decompressor :: Ptr Compressor -> IO ()
foreign import ccall unsafe "libdeflate_zlib_decompress" c_libdeflate_zlib_decompress :: Ptr Compressor -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt
