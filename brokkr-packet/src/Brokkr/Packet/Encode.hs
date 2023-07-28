{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Brokkr.Packet.Encode (
  toStrictByteString
, ToBinary
, Packet(..)
, SizeEstimate(..)
, module Brokkr.Packet.Settings
) where

import Brokkr.Packet.Binary
import Brokkr.Packet.Settings
import Brokkr.VarInt.Encode

import qualified Codec.Compression.Zlib as ZLib

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Lazy     qualified as LBS
import Data.IORef
import Data.Proxy

import Foreign.Ptr

import GHC.ForeignPtr
import GHC.Exts
import GHC.IO (IO(IO), unsafePerformIO)
import GHC.TypeLits

import Mason.Builder.Internal qualified as Mason

data Packet a = Packet !SizeEstimate !a
  deriving stock Show

newtype SizeEstimate = EstimateMin Int
  deriving stock Show

toStrictByteString :: forall compression a .
  ( KnownCompression compression, ToBinary a )
  => Proxy compression -> CompressionSettings -> EncryptionSettings -> Packet a -> ByteString
toStrictByteString cs compressionSettings0 _encryptionSettings (Packet (EstimateMin szEstimate) el) = unsafePerformIO $ do
  let compressionSettings  = compressionVal cs compressionSettings0
  -- This is almost an exact copy of toStrictByteString from mason, but handles prefixing the length, later on will deal with compression as well
  let prefixSz = case compressionSettings of
        NoCompression -> 5
        (UseCompression n) -> 1 + varIntSize (fromIntegral n) -- fast path, no compression needed writes 2 varints and the size is smaller than n

      finalSzEst = szEstimate + prefixSz
      initSz = (finalSzEst + 64 - 1) .&. complement (64 - 1) -- get some multiple of 64 larger than finalSzEst
  fptr0 <- mallocPlainForeignPtrBytes initSz
  bufRef <- newIORef fptr0
  let ptr0 = unsafeForeignPtrToPtr fptr0
  Mason.Buffer _ pos <- Mason.unBuilder (put el) (Mason.GrowingBuffer bufRef) (Mason.Buffer (plusPtr ptr0 initSz) (plusPtr ptr0 prefixSz))

  fptr <- readIORef bufRef
  let ptr = unsafeForeignPtrToPtr fptr
  let sz = minusPtr pos ptr - prefixSz

  -- All our data is now written to the buffer, now figure out what packet format with respect to compression we use

  -- Use no compression
  if | NoCompression <- compressionSettings -> do
        -- Calculate size of our prefix before writing
        let varSz = varIntSize sz 
            finalLen = sz + varSz
            pSize = prefixSz - varSz
        -- putStrLn "No compression"
        -- print (sz, varSz, pSize, finalLen)
        
        -- directly write the VarInt so that the data behind it is contiguous 
        -- We cannot the faster pdepEncodeVarInt here because that needs excess bytes
        -- off the end and we have data there!
        _ <- writeVarNumInternal (0xFFFFFFFF .&. fromIntegral sz) (plusPtr ptr pSize)

        -- print $ HexBS $ BS.BS (plusForeignPtr fptr pSize) finalLen

        pure $! BS.BS (plusForeignPtr fptr pSize) finalLen
      
     -- We use compression, but our packet was small enough to be send uncompressed
     | UseCompression n <- compressionSettings,
       fromIntegral n > sz -> do
        -- Our prefix is now the data size plus an additional 0 in between?
        let varSz = varIntSize (sz + 1)
            finalLen = sz + varSz + 1
            pOff = prefixSz - 1 - varSz
        -- putStrLn "Compression, below threshold"
        -- print (sz, varSz, pOff, finalLen)
        
        -- Size of the entire packet
        -- TODO Why +1? Does the uncompressed size count towards the compressed size's length?
        _ <- writeVarNumInternal (0xFFFFFFFF .&. fromIntegral (sz + 1)) (plusPtr ptr pOff)
        -- Size of the compressed bytes (always 0 here)
        case plusPtr ptr $ pOff + varSz of
          Ptr addr -> IO $ \s -> case writeInt8OffAddr# addr 0# (intToInt8# 0#) s of s' -> (# s', () #)

        -- print (prefixSz, varSz, sz, finalLen)
        -- print $ HexBS $ BS.BS fptr (sz + prefixSz)
        -- print $ HexBS $ BS.BS (plusForeignPtr fptr prefixSz) sz
        -- print $ HexBS $ BS.BS (plusForeignPtr fptr pOff) finalLen

        pure $! BS.BS (plusForeignPtr fptr pOff) finalLen
     -- We use compression and have to actually compress our data
     | otherwise -> do
      -- TODO can we do this over strict bytestrings directly and over buffers I control?
      -- That'd enable the prefix operation to be more efficient
        let (BS.BS fptrCompressed compressedSz) =
              LBS.toStrict . ZLib.compressWith (ZLib.defaultCompressParams { ZLib.compressLevel = ZLib.bestSpeed })
                $ LBS.fromStrict (BS.BS (plusForeignPtr fptr prefixSz) sz)
            pCompressedSize = varIntSize compressedSz
            pSize = pCompressedSize + varIntSize sz
            finSize = pSize + compressedSz
        -- putStrLn "Compression, above threshold"
        -- print (sz, pCompressedSize, pSize, finSize)

        withPrefix <- mallocPlainForeignPtrBytes finSize
        unsafeWithForeignPtr withPrefix $ \prefixPtr ->
          unsafeWithForeignPtr fptrCompressed $ \compressedPtr -> do

            _ <- writeVarNumInternal (0xFFFFFFFF .&. fromIntegral compressedSz) prefixPtr
            _ <- writeVarNumInternal (0xFFFFFFFF .&. fromIntegral sz) (plusPtr prefixPtr pCompressedSize)

            BS.memcpy (plusPtr prefixPtr pSize) compressedPtr compressedSz

        -- print $ HexBS $ BS.BS withPrefix finSize

        pure $! BS.BS withPrefix finSize
{-# SPECIALISE INLINE toStrictByteString
  :: forall thresh a . (KnownNat thresh, ToBinary a)
      => Proxy (UseCompression thresh) -> CompressionSettings -> EncryptionSettings -> Packet a -> ByteString  #-}
{-# SPECIALISE INLINE toStrictByteString
  :: forall a . ToBinary a
      => Proxy NoCompression -> CompressionSettings -> EncryptionSettings -> Packet a -> ByteString  #-}

varIntSize :: Int -> Int
varIntSize i
  | i < 0         = 5
  | i < 128       = 1
  | i < 16384     = 2
  | i < 2097152   = 3
  | i < 268435456 = 4
  | otherwise     = 5
