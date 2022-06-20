{-# LANGUAGE MultiWayIf #-}
module Network.Util.Builder (
  toStrictSizePrefixedByteString
) where

import Network.Util.VarNum

import Data.IORef
import Data.ByteString
import qualified Data.ByteString.Internal as BS
import Foreign
import Foreign.ForeignPtr.Unsafe
import GHC.IO (unsafePerformIO)
import qualified Mason.Builder.Internal as B
import Network.Protocol
import qualified Codec.Compression.Zlib as ZLib
import qualified Data.ByteString.Lazy as LBS

toStrictSizePrefixedByteString :: Protocol -> Int -> B.Builder -> ByteString
toStrictSizePrefixedByteString prot szEstimate bb = unsafePerformIO $ do
  -- This is almost an exact copy of toStrictByteString from mason, but handles prefixing the length, later on will deal with compression as well
  let prefixSz = case prot of
        Protocol NoCompression _ -> 5
        Protocol (Threshold n) _ -> 1 + varIntSize n -- fast path writes 2 varints
      
      finalSzEst = szEstimate + prefixSz
      initSz = (finalSzEst + 64 - 1) .&. (complement $ 64 - 1) -- get some multiple of 64 larger than finalSzEst
  fptr0 <- mallocForeignPtrBytes initSz
  bufRef <- newIORef fptr0
  let ptr0 = unsafeForeignPtrToPtr fptr0
  B.Buffer _ pos <- B.unBuilder bb (B.GrowingBuffer bufRef) (B.Buffer (plusPtr ptr0 initSz) (plusPtr ptr0 prefixSz))

  fptr <- readIORef bufRef
  let ptr = unsafeForeignPtrToPtr fptr
  let sz = (minusPtr pos ptr) - prefixSz

  -- All our data is now written to the buffer, now figure out what packet format with respect to compression we use

  -- Use no compression
  if | Protocol NoCompression _ <- prot -> do
         -- Calculate size of our prefix before writing
         let varSz = varIntSize sz 
             finalLen = sz + varSz
             pSize = prefixSz - varSz
         -- directly write the VarInt so that the data behind it is contiguous 
         _ <- writeVarNumInternal (fromIntegral sz) (plusPtr ptr pSize)

         pure $ BS.BS (plusForeignPtr fptr pSize) finalLen
      
     -- We use compression, but our packet was small enough to be send uncompressed
     | Protocol (Threshold n) _ <- prot,
       n > sz -> do
         -- Our prefix is now the data size plus an additional 0 in between
         let varSz = varIntSize $ sz + 1
             finalLen = sz + varSz + 1
             pSize = prefixSz - 1 - varSz
         _ <- writeVarNumInternal (fromIntegral sz + 1) (plusPtr ptr pSize)
         -- TODO just write a byte directly, no need to go through writeVarNum
         _ <- writeVarNumInternal 0 (plusPtr ptr $ pSize + varSz)
   
         pure $ BS.BS (plusForeignPtr fptr pSize) finalLen
     -- We use compression and have to actually compress our data
     | otherwise -> do
      -- TODO can we do this over strict bytestrings directly and over buffers I control?
      -- That'd enable the prefix operation to be more efficient
       let compressed =
            ZLib.compressWith (ZLib.defaultCompressParams { ZLib.compressLevel = ZLib.bestSpeed })
              $ LBS.fromStrict (BS.BS (plusForeignPtr fptr prefixSz) sz)
           cLen = varIntSize sz + (fromIntegral $ LBS.length compressed)
           pSize = varIntSize cLen + varIntSize sz

       prefix <- mallocForeignPtrBytes pSize
       let ptr' = unsafeForeignPtrToPtr prefix

       _ <- writeVarNumInternal (fromIntegral cLen) ptr'
       _ <- writeVarNumInternal (fromIntegral sz) (plusPtr ptr' (varIntSize cLen))

       pure . LBS.toStrict $ LBS.fromStrict (BS.BS prefix pSize) <> compressed
{-# INLINE toStrictSizePrefixedByteString #-}
