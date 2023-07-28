{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

import Test.Tasty.Bench

import Brokkr.Anvil.Chunk
import Brokkr.Anvil.Chunk.Section
import Brokkr.Anvil.Chunk.Parser
import Brokkr.Anvil.RegionFile

import Brokkr.NBT qualified as NBT
import Brokkr.NBT.Class qualified as NBT.Class
import Brokkr.NBT.Internal (NBT(..))

import Control.DeepSeq
import Control.Exception (evaluate)

import Codec.Compression.Zlib qualified as ZLib
import Codec.Compression.GZip qualified as GZip

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS

import Data.Int

import Data.Vector qualified as V

import FlatParse.Basic qualified as FP

import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)

import Mason.Builder qualified as Mason

instance NFData NBT where
  rnf NBT{} = ()
  {-# INLINE rnf #-}
instance NFData RegionFile where
  rnf RegionFile{} = ()
instance NFData ChunkPosition where
  rnf ChunkPosition{} = ()
instance NFData ChunkSection where
  rnf ChunkSection{} = ()
instance NFData Chunk where
  rnf Chunk{} = ()

{- 05.05.23

Full chunk loading takes 70μs with the schema generated parser (90μs without). It breaks down to:
- 1μs for loading from the region file (this may be affected by os page cache)
- 54μs for decompression
- 13μs for parsing into chunks (32μs without schema)

So ~80% of the time is spent decompressing data. Fun

Schema nbt parsing and encoding is almost optimal, only area of improvement in the nbt section is
probably parsing arbitrary tags and validating java cesu-8. Validating strings is less beneficial if
most keys are used (no need to validate those, since we validated them at compile time).  

So how do we improve decompression?
- Returning a lazy bytestring and thus skipping the allocation of the strict bytestring changes nothing,
  so it is most likely not an allocation problem
- Then zlib itself: try zlib-ng? First test was not that convincing, saved like 8μs at most, probably need
  to do a proper setup and test again

-}

main :: IO ()
main = defaultMain [
    envWithCleanup setupEnv closeEnv $ \ ~(Env{..}) ->
    bgroup "Chunk parsing"
      [ bench "IO ByteString" $ nfIO (readChunkData toLoad rFile (\s b -> pure (b,s)) (error "Failed to read"))
      , bench "Decompress ByteString" $ nf (\(s, bs) ->
            let decompress :: Int8 -> ByteString -> ByteString
                decompress 1 bs = LBS.toStrict . GZip.decompress $ LBS.fromStrict bs
                decompress 2 bs = LBS.toStrict . ZLib.decompress $ LBS.fromStrict bs
                decompress 3 bs = bs
                decompress _ _ = error "Decompresion failed"
            in decompress s bs
          ) (loadedCompScheme, loadedCompressed)
      , bench "ByteString -> NBT" $ nf parseNBT loadedDecompressed
      , bench "NBT -> Chunk" $ nf parseChunk nbt
      , bench "ByteString -> Chunk" $ nf parseChunkDirect loadedDecompressed
      , bench "ByteString -> Chunk (Schema)" $ nf parseChunkDirectSchema loadedDecompressed
      , bench "IO -> Chunk" $ nfIO (parseChunkIODirect rFile toLoad)
      , bench "IO -> Chunk (Schema)" $ nfIO (parseChunkIODirectSchema rFile toLoad)
      ]
  , envWithCleanup setupEnv closeEnv $ \ ~(Env{..}) ->
    bgroup "Chunk writing"
      [ bench "Chunk -> NBT" $ nf (NBT "" . NBT.Class.toNBT) chunk
      , bench "NBT -> ByteString" $ nf (\nbt -> Mason.toStrictByteString $ NBT.putNBT nbt) nbtFromChunk
      , bench "Chunk -> ByteString" $ nf (\c -> Mason.toStrictByteString $ NBT.putNBT (NBT "" $ NBT.Class.toNBT c)) chunk
      , bench "Chunk -> ByteString (Schema)" $ nf (\c -> Mason.toStrictByteString $ putChunkNBT c) chunk
      ]
  ]

data Env = Env {
  rFile :: !RegionFile
, toLoad :: !ChunkPosition
, loadedCompressed :: !ByteString
, loadedCompScheme :: !Int8
, loadedDecompressed :: !ByteString
, nbt :: !NBT
, section :: !ChunkSection
, chunk :: !Chunk
, nbtFromChunk :: !NBT
}
  deriving stock Generic
  deriving anyclass NFData

closeEnv :: Env -> IO ()
closeEnv Env{rFile} = closeRegionFile rFile

setupEnv :: IO Env
setupEnv = do
  let fp = "benchmark"
      toLoad = ChunkPosition 0 0
      rFileToLoad = toRfileCoords toLoad
  rFile <- openRFile fp rFileToLoad
  (loadedCompressed, loadedCompScheme)
    <- readChunkData toLoad rFile (\s b -> pure (b,s)) (error "Failed to read")
  loadedDecompressed <- loadChunkData rFile toLoad
  let chunk = parseChunk nbt
      nbt = parseNBT loadedDecompressed
  let section = V.head $ chunkSections chunk
      nbtFromChunk = NBT "" $ NBT.Class.toNBT chunk
  pure Env{..}

loadChunkData :: RegionFile -> ChunkPosition -> IO ByteString
loadChunkData rFile = loadCData
  where
    decompress 1 bs = pure $! LBS.toStrict . GZip.decompress $ LBS.fromStrict bs
    decompress 2 bs = pure $! LBS.toStrict . ZLib.decompress $ LBS.fromStrict bs
    decompress 3 bs = pure $! bs
    decompress _ _ = error "Decompresion failed"
    loadCData cp = readChunkData cp rFile decompress (error "Failed to read")
{-# INLINE loadChunkData #-}

parseChunkIODirect :: RegionFile -> ChunkPosition -> IO Chunk
parseChunkIODirect rFile = loadCData
  where
    decompress 1 bs = pure $! LBS.toStrict . GZip.decompress $ LBS.fromStrict bs
    decompress 2 bs = pure $! LBS.toStrict . ZLib.decompress $ LBS.fromStrict bs
    decompress 3 bs = pure $! bs
    decompress _ _ = error "Decompresion failed"
    loadCData cp = do
      bs <- readChunkData cp rFile decompress (error "Failed to read")
      case FP.runParser parseNBTAndChunk bs of
        FP.OK c _ -> evaluate c
        _ -> error ""
{-# INLINE parseChunkIODirect #-}

parseChunkIODirectSchema :: RegionFile -> ChunkPosition -> IO Chunk
parseChunkIODirectSchema rFile = loadCData
  where
    decompress 1 bs = pure $! LBS.toStrict . GZip.decompress $ LBS.fromStrict bs
    decompress 2 bs = pure $! LBS.toStrict . ZLib.decompress $ LBS.fromStrict bs
    decompress 3 bs = pure $! bs
    decompress _ _ = error "Decompresion failed"
    loadCData cp = do
      bs <- readChunkData cp rFile decompress (error "Failed to read")
      case FP.runParser parseChunkNBT bs of
        FP.OK c _ -> evaluate c
        _ -> error ""
{-# INLINE parseChunkIODirectSchema #-}

openRFile :: String -> (Int, Int) -> IO RegionFile
openRFile fp = uncurry (openRegionFile fp)
{-# INLINE openRFile #-}

toRfileCoords :: ChunkPosition -> (Int, Int)
toRfileCoords (ChunkPosition x z) =
  let rX = x `div` 32
      rZ = z `div` 32
  in (fromIntegral rX, fromIntegral rZ) 
{-# INLINE toRfileCoords #-}

parseNBT :: ByteString -> NBT
parseNBT = getNBT
  where
    getNBT bs = case FP.runParser NBT.parseNBT bs of
      FP.OK nbt _ -> nbt
      _ -> error ""
{-# INLINE parseNBT #-}

parseChunk :: NBT -> Chunk
parseChunk (NBT _ tag) = case NBT.Class.runParser (NBT.Class.fromNBT "Chunk" tag) tag of
  Left _ -> error "Failed"
  Right !x -> x 
{-# INLINE parseChunk #-}

parseChunkDirect :: ByteString -> Chunk
parseChunkDirect = parseChunk
  where
    parseChunk bs = case FP.runParser parseNBTAndChunk bs of
      FP.OK c _ -> c
      _ -> error ""
{-# INLINE parseChunkDirect #-}

parseChunkDirectSchema :: ByteString -> Chunk
parseChunkDirectSchema = parseChunk
  where
    parseChunk bs = case FP.runParser parseChunkNBT bs of
      FP.OK c _ -> c
      _ -> error ""
{-# INLINE parseChunkDirectSchema #-}

parseNBTAndChunk :: FP.Parser NBT.NBTError Chunk
parseNBTAndChunk = NBT.parseNBT >>= \(NBT _ t) -> do
  case NBT.Class.runParser (NBT.Class.fromNBT "Chunk" t) t of
    Right r -> pure r
    Left  e -> FP.err e
{-# INLINE parseNBTAndChunk #-}

