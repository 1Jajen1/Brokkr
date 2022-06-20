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
import qualified Foreign.Storable as S
import Chunk.Position
import qualified FlatParse.Basic as FP
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.Zlib as ZLib
import Data.Int
import Util.Binary
import qualified Codec.Compression.GZip as GZip
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
  when (sz /= sectorSize) $ error "Invalid location table size" -- TODO Error
  let locationTable = S.unsafeFromForeignPtr0 (coerce fptr) $ sectorSize `div` (S.sizeOf @ChunkLocation_ undefined)
  pure $! RegionFile{..}

readChunkData :: ChunkPosition -> RegionFile -> IO (Maybe ByteString)
readChunkData (ChunkPos x z) RegionFile{..}
  | off == 0 && sz == 0 = pure Nothing
  | otherwise = do
    compressedBs <- readAt file (unsafeShiftL (fromIntegral off) 12) (unsafeShiftL (fromIntegral sz) 12)
    case FP.runParser chunkDataP compressedBs of
      FP.OK res _ -> pure $ Just res
      _ -> error "Failed decompressing chunk data" -- TODO Error
    where
      rIndex = x .&. 31 + unsafeShiftL (z .&. 31) 5
      (ChunkLocation off sz) = locationTable `S.unsafeIndex` rIndex
      chunkDataP = do
        sizePre <- get @Int32
        compType <- get @Int8
        bs <- takeBs . fromIntegral $ sizePre - 1
        case compType of
          2 -> pure $ LBS.toStrict . ZLib.decompress $ LBS.fromStrict bs
          1 -> pure $ LBS.toStrict . GZip.decompress $ LBS.fromStrict bs
          3 -> pure bs
          _ -> error "Unknown compression scheme" -- TODO error

closeRegionFile :: RegionFile -> IO ()
closeRegionFile RegionFile{file} = hClose file
