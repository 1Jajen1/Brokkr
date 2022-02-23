{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Effect.IO.RegionFile (
  RegionFile
, openRegionFile
, readChunkData
, closeRegionFile
) where

import Effectful
import qualified Data.Vector.Storable as S
import qualified Data.ByteString.Internal as BS
import Data.Word
import Data.Bits
import Util.Word24
import Util.ByteOrder
import Effect.IO.File
import Prelude hiding (FilePath)
import Control.Monad
import Data.Coerce
import Data.ByteString
import qualified Foreign.Storable as S
import Chunk.Position
import qualified FlatParse.Basic as FP
import Util.Flatparse (takeN)
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.Zlib as ZLib
import Data.Int
import Util.Binary
import qualified Codec.Compression.GZip as GZip
import Effect.World (RegionFileFolderPath (RegionFileFolderPath))
import Effectful.Reader.Static
import Data.String
import Effect.Token

data RegionFile file = RegionFile {
  locationTable :: !(S.Vector ChunkLocation_)
-- at some point we may also need to do ChunkTimestamps but not now...
, file          :: !file
}

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
-- TODO Double check endianess...

openRegionFile ::
  forall file w es .
  ( Reader (RegionFileFolderPath w) :> es
  , File file :> es
  ) => Token w -> Int -> Int -> Eff es (RegionFile file)
openRegionFile _ regionX regionZ = do
  RegionFileFolderPath folder <- ask @(RegionFileFolderPath w)
  let path = folder <> "/r." <> fromString (show regionX) <> "." <> fromString (show regionZ) <> ".mca"
  file <- openAt path OpenReadWrite
  (BS.BS fptr sz) <- readAt 0 sectorSize file
  when (sz /= sectorSize) $ error "Invalid location table size" -- TODO Error
  let locationTable = S.unsafeFromForeignPtr0 (coerce fptr) $ sectorSize `div` (S.sizeOf @ChunkLocation_ undefined)
  pure $ RegionFile{..}

readChunkData :: File file :> es => ChunkPosition -> RegionFile file -> Eff es ByteString
readChunkData (ChunkPos x z) RegionFile{..} = do
  compressedBs <- readAt (unsafeShiftL (fromIntegral off) 12) (unsafeShiftL (fromIntegral sz) 12) file
  case FP.runParser chunkDataP compressedBs of
    FP.OK res _ -> pure res
    _ -> error "Failed decompressing chunk data" -- TODO Error
  where
    rIndex = x .&. 31 + unsafeShiftL (z .&. 31) 5
    -- unsafeIndex is safe as long as rIndex is guaranteed to fall between 0-1024. Since 31*31 < 32*32 <=> 31*31 < 1024 the upper bound is correct.
    -- the number also has to be positive, which follows trivially from how .&. (and) works, and obv 31 * 5 cannot overflow 32/64 bit numbers.
    (ChunkLocation off sz) = locationTable `S.unsafeIndex` rIndex
    chunkDataP = do
      sizePre <- get @Int32
      compType <- get @Int8
      bs <- takeN . fromIntegral $ sizePre - 1
      case compType of
        2 -> pure $ LBS.toStrict . ZLib.decompress $ LBS.fromStrict bs
        1 -> pure $ LBS.toStrict . GZip.decompress $ LBS.fromStrict bs
        3 -> pure bs
        _ -> error "Unknown compression scheme" -- TODO error

closeRegionFile :: File file :> es => RegionFile file -> Eff es ()
closeRegionFile RegionFile{file} = close file
