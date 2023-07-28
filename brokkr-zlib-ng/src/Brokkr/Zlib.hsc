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
import Foreign.Storable (peek, poke)

import GHC.ForeignPtr
import GHC.Ptr

#include "zlib-ng.h"

decompressIO :: Int -> ByteString -> IO ByteString
decompressIO decompressedSz (BS.BS srcFPtr compressedSz) = do
  dstFPtr <- BS.mallocByteString decompressedSz
  unsafeWithForeignPtr srcFPtr $ \srcPtr ->
    unsafeWithForeignPtr dstFPtr $ \dstPtr ->
      alloca $ \dstSzPtr -> do
        poke dstSzPtr $ fromIntegral decompressedSz
        c_zng_uncompress dstPtr dstSzPtr srcPtr (fromIntegral compressedSz) >>= \case
          #{const Z_OK} -> do
            resSz <- fromIntegral <$> peek dstSzPtr
            pure $ BS.BS dstFPtr resSz
          #{const Z_MEM_ERROR} -> throwIO ZMemError
          #{const Z_BUF_ERROR} -> throwIO ZBuffError
          #{const Z_DATA_ERROR} -> throwIO ZBuffError
          res -> throwIO $ ZUnknown (fromIntegral res)
{-# INLINE decompressIO #-}

data ZError = ZMemError | ZBuffError | ZDataError | ZUnknown Int 
  deriving stock Show
  deriving anyclass Exception

foreign import ccall unsafe "zng_uncompress" c_zng_uncompress :: Ptr Word8 -> Ptr CSize -> Ptr Word8 -> CSize -> IO CInt

