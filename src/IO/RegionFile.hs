{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module IO.RegionFile (
  RegionFile
, openRegionFile
, readChunkData
, closeRegionFile
) where

import qualified Data.Vector.Storable as S
import qualified Data.ByteString.Internal as BS
import Data.Word
import Data.Bits
import Util.Word24
import Util.ByteOrder
import Prelude hiding (FilePath)
import Control.Monad
import Data.Coerce
import Data.ByteString
import Control.Exception
import qualified Foreign.Storable as S
import Chunk.Position
import qualified FlatParse.Basic as FP
import Data.Int
import Util.Binary
import Data.String
import System.IO
import FlatParse.Basic
import Control.DeepSeq

data RegionFile = RegionFile {
  locationTable :: !(S.Vector ChunkLocation_)
-- at some point we may also need to do ChunkTimestamps but not now...
, file          :: !Handle
}

instance NFData RegionFile where
  rnf !RegionFile{} = ()

sectorSize :: Int
sectorSize = 4096

newtype ChunkLocation_ = ChunkLocation_ Word32
  deriving newtype S.Storable

instance Show ChunkLocation_ where
  show (ChunkLocation off sz) = "ChunkLocation " <> show off <> " " <> show sz

pattern ChunkLocation :: Word24 -> Word8 -> ChunkLocation_
pattern ChunkLocation off sz <- !(toOffAndSz -> (off, sz)) where
  ChunkLocation !off !sz = ChunkLocation_ $ toBE (unsafeShiftL (fromIntegral off) 8) .|. (fromIntegral sz)
{-# COMPLETE ChunkLocation #-}
{-# INLINE ChunkLocation #-}

toOffAndSz :: ChunkLocation_ -> (Word24, Word8)
toOffAndSz (ChunkLocation_ x) = let x' = toBE x in (fromIntegral $ unsafeShiftR x' 8, fromIntegral $ x' .&. 0x000000ff)
{-# INLINE toOffAndSz #-}

readAt :: Handle -> Int -> Int -> IO ByteString
readAt hdl off sz = do
  hSeek hdl AbsoluteSeek $ toInteger off
  hGet hdl sz
{-# INLINE readAt #-}

openRegionFile :: String -> Int -> Int -> IO RegionFile
openRegionFile folder regionX regionZ = do
  let path = folder <> "/r." <> fromString (show regionX) <> "." <> fromString (show regionZ) <> ".mca"
  file <- openBinaryFile path ReadWriteMode
  (BS.BS fptr sz) <- readAt file 0 sectorSize
  when (sz /= sectorSize) $ throwIO $ InvalidLocationTable path regionX regionZ
  let locationTable = S.unsafeFromForeignPtr0 (coerce fptr) $ sectorSize `div` (S.sizeOf @ChunkLocation_ undefined)
  pure $! RegionFile{..}

data InvalidLocationTable = InvalidLocationTable String Int Int
  deriving stock Show

instance Exception InvalidLocationTable 

readChunkData :: ChunkPosition -> RegionFile -> (Int8 -> ByteString -> IO a) -> IO a -> IO a
readChunkData (ChunkPos x z) RegionFile{..} cont notPresent
  | off == 0 && sz == 0 = notPresent
  | otherwise = do
    compressedBs <- readAt file (unsafeShiftL (fromIntegral off) 12) (unsafeShiftL (fromIntegral sz) 12)
    case FP.runParser chunkDataP compressedBs of
      FP.OK (!compTy, !bs) _ -> cont compTy bs
      _ -> throwIO $ FailedReadingCompressedChunk (ChunkPos x z)
    where
      rIndex = x .&. 31 + 32 * (z .&. 31)
      (ChunkLocation off sz) = locationTable `S.unsafeIndex` rIndex
      chunkDataP = do
        sizePre <- get @Int32
        compType <- get @Int8
        !bs <- takeBs . fromIntegral $ sizePre - 1
        pure (compType, bs)
        
{-# INLINE readChunkData #-}

closeRegionFile :: RegionFile -> IO ()
closeRegionFile RegionFile{file} = hClose file

newtype FailedReadingCompressedChunk = FailedReadingCompressedChunk ChunkPosition
  deriving stock Show

instance Exception FailedReadingCompressedChunk
