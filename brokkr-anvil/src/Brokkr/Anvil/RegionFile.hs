{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
module Brokkr.Anvil.RegionFile (
  RegionFile(..)
, openRegionFile
, readChunkData
, closeRegionFile
, FailedReadingCompressedChunk(..)
, InvalidLocationTable(..)
) where

import Brokkr.Anvil.Chunk

import Control.Exception
import Control.Monad (when)

import Data.Bits

import Data.ByteString (ByteString, hGet)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Unsafe qualified as BS

import Data.Int
import Data.Word

import Data.Vector.Storable qualified as S

import Foreign.Storable qualified as S

import GHC.Exts
import GHC.IO

import System.IO
import GHC.Int


data RegionFile = RegionFile {
  locationTable :: !(S.Vector ChunkLocation_)
-- at some point we may also need to do ChunkTimestamps but not now...
-- TODO abstract away the file to allow more sources
, file          :: !Handle
}

sectorSize :: Int
sectorSize = 4096

newtype ChunkLocation_ = ChunkLocation_ Word32
  deriving newtype S.Storable

instance Show ChunkLocation_ where
  show (ChunkLocation off sz) = "ChunkLocation " <> show off <> " " <> show sz

pattern ChunkLocation :: Word -> Word8 -> ChunkLocation_
#ifdef WORDS_BIGENDIAN
pattern ChunkLocation off sz <- !((\(ChunkLocation_ x) -> (fromIntegral $ x `unsafeShiftR` 8, fromIntegral $ x .&. 0x000000ff)) -> (off, sz)) where
  ChunkLocation !off !sz = ChunkLocation_ $ (fromIntegral off `unsafeShiftL` 8) .|. fromIntegral sz
#else
pattern ChunkLocation off sz <- !((\(ChunkLocation_ x0) ->
    let x :: Word32 = byteSwap32 x0 in (fromIntegral $ x `unsafeShiftR` 8, fromIntegral $ x .&. 0x000000ff)
  ) -> (off, sz)) where
  ChunkLocation !off !sz = ChunkLocation_ $ byteSwap32 (fromIntegral off `unsafeShiftL` 8) .|. fromIntegral sz
#endif
{-# COMPLETE ChunkLocation #-}
{-# INLINE ChunkLocation #-}

newtype FailedReadingCompressedChunk = FailedReadingCompressedChunk ChunkPosition
  deriving stock Show
  deriving anyclass Exception

data InvalidLocationTable = InvalidLocationTable String Int Int
  deriving stock Show
  deriving anyclass Exception

readAt :: Handle -> Int -> Int -> IO ByteString
{-# INLINE readAt #-}
readAt hdl off sz = do
  hSeek hdl AbsoluteSeek $ toInteger off
  hGet hdl sz

openRegionFile :: String -> Int -> Int -> IO RegionFile
openRegionFile folder regionX regionZ = do
  let path = folder <> "/r." <> fromString (show regionX) <> "." <> fromString (show regionZ) <> ".mca"
  file <- openBinaryFile path ReadWriteMode
  (BS.BS fptr sz) <- readAt file 0 sectorSize
  when (sz /= sectorSize) $ throwIO $ InvalidLocationTable path regionX regionZ
  let locationTable = S.unsafeFromForeignPtr0 (coerce fptr) $ sectorSize `quot` S.sizeOf @ChunkLocation_ undefined
  pure $! RegionFile{..}

readChunkData :: ChunkPosition -> RegionFile -> (Int8 -> ByteString -> IO a) -> IO a -> IO a
{-# INLINE readChunkData #-}
readChunkData (ChunkPosition x z) RegionFile{..} cont notPresent
  | off == 0 && sz == 0 = notPresent
  | otherwise = do
    compressedBs <- readAt file (fromIntegral off * sectorSize) (fromIntegral sz * sectorSize)
    BS.unsafeUseAsCStringLen compressedBs $ \case
      (Ptr addr, cSz@(I# cSz#))
        | cSz <= 4 -> throwIO $ FailedReadingCompressedChunk (ChunkPosition x z)
        | otherwise ->
#ifdef WORDS_BIGENDIAN
          let compressedSz = int32ToInt# (indexInt32OffAddr# addr 0#)
              compressionType = indexInt8OffAddr# addr 4#
#else
          let compressedSz = word2Int# (byteSwap32# (word32ToWord# (indexWord32OffAddr# addr 0#)))
              compressionType = indexInt8OffAddr# addr 4#
#endif
          in if isTrue# (compressedSz <=# (cSz# -# 4#)) -- The compression type is included in the length ffs
            then cont (I8# compressionType) (BS.take (I# compressedSz) $ BS.drop 5 compressedBs)
            else throwIO $ FailedReadingCompressedChunk (ChunkPosition x z)
    where
      rIndex = fromIntegral $ x .&. 31 + 32 * (z .&. 31)
      (ChunkLocation off sz) = locationTable `S.unsafeIndex` rIndex

closeRegionFile :: RegionFile -> IO ()
closeRegionFile RegionFile{file} = hClose file
