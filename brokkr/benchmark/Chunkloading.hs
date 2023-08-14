{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PackageImports #-}
module Main (main) where

import Test.Tasty.Bench

import Control.Concurrent.MVar
import Data.ByteString
import qualified FlatParse.Basic as FP
import Chunk.Position
import IO.Chunk hiding (countBlocks)
import qualified IO.Chunk
import qualified IO.Chunkloading as CL
import IO.RegionFile
import "brokkr" Util.NBT
import "brokkr" Util.NBT.Internal
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import GHC.Generics
import Control.DeepSeq
import Data.Foldable
import Data.List (nub)
import Util.Binary
import Data.Maybe
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.Zlib as ZLib
import qualified Codec.Compression.GZip as GZip
import Data.Monoid

instance NFData NBT where
  rnf (NBT _ _) = ()
  {-# INLINE rnf #-}

-- TODO Rework this to benchmark Anvil.Chunk -> Chunk and Chunk -> PlayPacket conversion

main :: IO ()
main = defaultMain [
    -- TODO Bench the chunkloading handle
    envWithCleanup setupEnv closeEnv $ \ ~(Env{..}) ->
    bgroup "Chunk parsing and sending (multiple)" $
      [ -- bench "Chunk -> ChunkData" $ nf mkChunkDataBench chunks
      -- Figure out what is slowing us down here
      -- 1. Buffer allocation?
      -- 2. Actual writes? If so which?
      -- Also test if compression can be sped up by using the c lib directly over the same buffer
      -- compressing in place
      -- Experiment with caching the full packet?
      -- , bench "ChunkData -> ByteString" $ nf encodeChunkData chunkData
        bench "IO ByteString" $ nfIO (loadChunkData rFiles toLoad)
      , bench "ByteString -> NBT" $ nf parseNBTs loaded
      , bench "NBT -> Chunk" $ nf parseChunks nbts
      , bench "countBlocks" $ nf countBlocks chunks
      -- write the chunk binary instance to operate on the bytestring directly without an intermediate
      -- NBT parse
      , bench "ByteString -> Chunk" $ nf parseChunksDirect loaded
      , bench "ByteString -> Chunk (Schema)" $ nf parseChunksDirectSchema loaded
      , bench "IO -> Chunk" $ nfIO (parseChunksIODirect rFiles toLoad)
      , bench "IO -> Chunk (Schema)" $ nfIO (parseChunksIODirectSchema rFiles toLoad)
      ]
  , envWithCleanup setupEnv1 closeEnv1 $ \ ~(Env1{..}) ->
    bgroup "Chunk parsing and sending (one)" $
      [ -- bench "Chunk -> ChunkData" $ nf mkChunkDataBench chunks
      -- Figure out what is slowing us down here
      -- 1. Buffer allocation?
      -- 2. Actual writes? If so which?
      -- Also test if compression can be sped up by using the c lib directly over the same buffer
      -- compressing in place
      -- Experiment with caching the full packet?
      -- , bench "ChunkData -> ByteString" $ nf encodeChunkData chunkData
        bench "IO ByteString" $ nfIO (loadChunkData1 rFiles1 toLoad1)
      , bench "ByteString -> NBT" $ nf parseNBTs1 loaded1
      , bench "NBT -> Chunk" $ nf parseChunks1 nbts1
      , bench "countBlocks" $ nf countBlocks1 section1
      -- write the chunk binary instance to operate on the bytestring directly without an intermediate
      -- NBT parse
      , bench "ByteString -> Chunk" $ nf parseChunksDirect1 loaded1
      , bench "ByteString -> Chunk (Schema)" $ nf parseChunksDirectSchema1 loaded1
      , bench "IO -> Chunk" $ nfIO (parseChunksIODirect1 rFiles1 toLoad1)
      , bench "IO -> Chunk (Schema)" $ nfIO (parseChunksIODirectSchema1 rFiles1 toLoad1)
      ]
  -- TODO This benchmark won't work till I add some way of completion back in
  -- , envWithCleanup setupCLEnv (CL.close . hdl) $ \ ~(CLEnv{..}) -> bench "IO Chunk" . nfIO $ CL.loadChunks hdl toLoad1 (const $ pure ())
  ]

{- 17.01.23 Notes:

Disk to fully parsed chunks takes ~80ms for a viewDistance of 10.
- 50ms of that is the IO in the regionfile (find + read + decompress)
  - uncompressed read is only 6.5ms, so decompression is 43.5ms
- 27ms is the parse
  - 1ms for block counting

So ~50% of the time is spent decompressing (ZLib in this case):
  - Questions:
    - Is it the actual compression that is problematic or the conversion strict/lazy bytestring?
      - My guess is actually zlib, not the strict/lazy conversion because the lazy bytestring should be one chunk only in both cases
    - Are there faster implementations out there? The zlib haskell package binds to https://github.com/madler/zlib
      - Yes: zlib-ng. So write a small very low level lib to use it... -- Fun cmake and related are awful and not straightforward in how to actually use them

On the 27ms parsing:
  - It is a bit weird where the cost actually is (BS -> NBT or NBT -> Chunk) GHC seems to be really good at fusing the two
  - Stuff to write/test:
    - Parse nbt as a state machine sort of parser, ie have one top level loop and case on the type/id
      let parse :: b -> x1 -> ... -> xN -> A
          parse b x1 ... xN | done b = A x1 ... xN
          parse n x1 ... xN =
            case next of
              xI -> parse (add n) x1 ... xI ... xN
      - This should be as efficient as it gets
      - How do we avoid boxing overhead? (we have to pass error to the components?)
        - manually unbox wherever possible and use some defaults
      - This needs TH? (I can probably also write a Generics version but TH is easier and easier on GHC to compile)
    - Other option is to write into a SmallArray and ByteArray. Should also be fine, just needs two small allocations
      - ^^ Try this first
  - I also will need to copy Strings/Arrays once more to no retain the (large) chunk from decompression 
-}

-- setupCLEnv :: IO CLEnv
-- setupCLEnv = do
--   hdl <- CL.new 16 -- "benchmark/Chunkloading/region"
--   let viewDistance = 10
--       toLoad1 = V.fromList [ChunkPos x z | x <- [-viewDistance..viewDistance], z <- [-viewDistance..viewDistance]]
--   pure CLEnv{..}

-- data CLEnv = CLEnv {
--   toLoad1 :: !(V.Vector ChunkPosition)
-- , hdl :: !CL.Chunkloading
-- }
--   deriving stock Generic
--   deriving anyclass NFData

data Env = Env {
  rFiles    :: !(HM.HashMap (Int, Int) RegionFile)
, toLoad    :: !(V.Vector ChunkPosition)
, loaded    :: !(V.Vector ByteString)
, nbts      :: !(V.Vector NBT)
, chunks    :: !(V.Vector Chunk)
-- , chunkData :: !(V.Vector (Int, ChunkData))
}
  deriving stock Generic
  deriving anyclass NFData

closeEnv :: Env -> IO ()
closeEnv Env{rFiles} = traverse_ closeRegionFile rFiles

setupEnv :: IO Env
setupEnv = do
  let fp = "benchmark/Chunkloading/region"
      viewDistance = 10
      toLoad = V.fromList [ChunkPos x z | x <- [-viewDistance..viewDistance], z <- [-viewDistance..viewDistance]]
      rFilesToLoad =  nub $ fmap toRfileCoords $ V.toList toLoad
  rFiles <- fmap HM.fromList $ traverse (\cp -> fmap (cp,) $ openRFile fp cp) rFilesToLoad
  loaded <- loadChunkData rFiles toLoad
  let chunks = parseChunks nbts
      -- chunkData = mkChunkDataBench chunks
      nbts = parseNBTs loaded
  pure Env{..}

data Env1 = Env1 {
  rFiles1    :: !RegionFile
, toLoad1    :: !ChunkPosition
, loaded1    :: !ByteString
, nbts1      :: !NBT
, section1   :: !ChunkSection
, chunks1    :: !Chunk
-- , chunkData :: !(V.Vector (Int, ChunkData))
}
  deriving stock Generic
  deriving anyclass NFData

closeEnv1 :: Env1 -> IO ()
closeEnv1 Env1{rFiles1} = closeRegionFile rFiles1

setupEnv1 :: IO Env1
setupEnv1 = do
  let fp = "benchmark/Chunkloading/region"
      toLoad1 = ChunkPos 0 0
      rFilesToLoad = toRfileCoords toLoad1
  rFiles1 <- openRFile fp rFilesToLoad
  loaded1 <- loadChunkData1 rFiles1 toLoad1
  let chunks1 = parseChunks1 nbts1
      -- chunkData = mkChunkDataBench chunks
      nbts1 = parseNBTs1 loaded1
  let section1 = V.head $ sections chunks1
  pure Env1{..}

-- mkChunkDataBench :: V.Vector Chunk -> V.Vector (Int, ChunkData)
-- mkChunkDataBench = fmap mkChunkData
-- {-# INLINE mkChunkDataBench #-}

-- encodeChunkData :: V.Vector (Int, ChunkData) -> V.Vector ByteString
-- encodeChunkData = fmap $ \(sz, cd) -> toStrictSizePrefixedByteString (Protocol NoCompression NoEncryption) sz (put $ ChunkDataAndUpdateLight cd)
-- {-# INLINE encodeChunkData #-}

loadChunkData :: HM.HashMap (Int, Int) RegionFile -> V.Vector ChunkPosition -> IO (V.Vector ByteString)
loadChunkData rFiles = traverse loadCData
  where
    decompress 1 bs = pure $! LBS.toStrict . GZip.decompress $ LBS.fromStrict bs
    decompress 2 bs = pure $! LBS.toStrict . ZLib.decompress $ LBS.fromStrict bs
    decompress 3 bs = pure $! bs
    decompress _ _ = error "Decompresion failed"
    loadCData cp =
      let rp = toRfileCoords cp
          Just rFile = HM.lookup rp rFiles
      in readChunkData cp rFile decompress (error "Failed to read")
{-# INLINE loadChunkData #-}

loadChunkData1 :: RegionFile -> ChunkPosition -> IO ByteString
loadChunkData1 rFile = loadCData
  where
    decompress 1 bs = pure $! LBS.toStrict . GZip.decompress $ LBS.fromStrict bs
    decompress 2 bs = pure $! LBS.toStrict . ZLib.decompress $ LBS.fromStrict bs
    decompress 3 bs = pure $! bs
    decompress _ _ = error "Decompresion failed"
    loadCData cp =
      let rp = toRfileCoords cp
      in readChunkData cp rFile decompress (error "Failed to read")
{-# INLINE loadChunkData1 #-}

parseChunksIODirect :: HM.HashMap (Int, Int) RegionFile -> V.Vector ChunkPosition -> IO (V.Vector Chunk)
parseChunksIODirect rFiles = traverse loadCData
  where
    decompress 1 bs = pure $! LBS.toStrict . GZip.decompress $ LBS.fromStrict bs
    decompress 2 bs = pure $! LBS.toStrict . ZLib.decompress $ LBS.fromStrict bs
    decompress 3 bs = pure $! bs
    decompress _ _ = error "Decompresion failed"
    loadCData cp =
      let rp = toRfileCoords cp
          Just rFile = HM.lookup rp rFiles
      in do
        bs <- readChunkData cp rFile decompress (error "Failed to read")
        pure $ case FP.runParser (get @(BinaryNBT Chunk)) bs of
          FP.OK (BinaryNBT c) _ -> c
          _ -> error ""
{-# INLINE parseChunksIODirect #-}

parseChunksIODirectSchema :: HM.HashMap (Int, Int) RegionFile -> V.Vector ChunkPosition -> IO (V.Vector Chunk)
parseChunksIODirectSchema rFiles = traverse loadCData
  where
    decompress 1 bs = pure $! LBS.toStrict . GZip.decompress $ LBS.fromStrict bs
    decompress 2 bs = pure $! LBS.toStrict . ZLib.decompress $ LBS.fromStrict bs
    decompress 3 bs = pure $! bs
    decompress _ _ = error "Decompresion failed"
    loadCData cp =
      let rp = toRfileCoords cp
          Just rFile = HM.lookup rp rFiles
      in do
        bs <- readChunkData cp rFile decompress (error "Failed to read")
        pure $ case FP.runParser (get @Chunk) bs of
          FP.OK c _ -> c
          _ -> error ""
{-# INLINE parseChunksIODirectSchema #-}

parseChunksIODirect1 :: RegionFile -> ChunkPosition -> IO Chunk
parseChunksIODirect1 rFile = loadCData
  where
    decompress 1 bs = pure $! LBS.toStrict . GZip.decompress $ LBS.fromStrict bs
    decompress 2 bs = pure $! LBS.toStrict . ZLib.decompress $ LBS.fromStrict bs
    decompress 3 bs = pure $! bs
    decompress _ _ = error "Decompresion failed"
    loadCData cp =
      let rp = toRfileCoords cp
      in do
        bs <- readChunkData cp rFile decompress (error "Failed to read")
        pure $ case FP.runParser (get @(BinaryNBT Chunk)) bs of
          FP.OK (BinaryNBT c) _ -> c
          _ -> error ""
{-# INLINE parseChunksIODirect1 #-}


parseChunksIODirectSchema1 :: RegionFile -> ChunkPosition -> IO Chunk
parseChunksIODirectSchema1 rFile = loadCData
  where
    decompress 1 bs = pure $! LBS.toStrict . GZip.decompress $ LBS.fromStrict bs
    decompress 2 bs = pure $! LBS.toStrict . ZLib.decompress $ LBS.fromStrict bs
    decompress 3 bs = pure $! bs
    decompress _ _ = error "Decompresion failed"
    loadCData cp =
      let rp = toRfileCoords cp
      in do
        bs <- readChunkData cp rFile decompress (error "Failed to read")
        pure $ case FP.runParser (get @Chunk) bs of
          FP.OK c _ -> c
          _ -> error ""
{-# INLINE parseChunksIODirectSchema1 #-}

openRFile :: String -> (Int, Int) -> IO RegionFile
openRFile fp = uncurry (openRegionFile fp)
{-# INLINE openRFile #-}

toRfileCoords :: ChunkPosition -> (Int, Int)
toRfileCoords (ChunkPos x z) =
  let rX = x `div` 32
      rZ = z `div` 32
  in (rX, rZ) 
{-# INLINE toRfileCoords #-}

parseNBTs :: V.Vector ByteString -> V.Vector NBT
parseNBTs = fmap getNBT
  where
    getNBT bs = case FP.runParser (get @NBT) bs of
      FP.OK nbt _ -> nbt
      _ -> error ""
{-# INLINE parseNBTs #-}

parseNBTs1 :: ByteString -> NBT
parseNBTs1 = getNBT
  where
    getNBT bs = case FP.runParser (get @NBT) bs of
      FP.OK nbt _ -> nbt
      _ -> error ""
{-# INLINE parseNBTs1 #-}

parseChunks :: V.Vector NBT -> V.Vector Chunk
parseChunks = fmap (\(NBT _ tag) -> runParser (parseNBT tag) id (error "Woops") tag)
{-# INLINE parseChunks #-}

parseChunks1 :: NBT -> Chunk
parseChunks1 = (\(NBT _ tag) -> runParser (parseNBT tag) id (error "Woops") tag)
{-# INLINE parseChunks1 #-}

parseChunksDirect :: V.Vector ByteString -> V.Vector Chunk
parseChunksDirect = fmap parseChunk
  where
    parseChunk bs = case FP.runParser (get @(BinaryNBT Chunk)) bs of
      FP.OK (BinaryNBT c) _ -> c
      _ -> error ""
{-# INLINE parseChunksDirect #-}

parseChunksDirect1 :: ByteString -> Chunk
parseChunksDirect1 = parseChunk
  where
    parseChunk bs = case FP.runParser (get @(BinaryNBT Chunk)) bs of
      FP.OK (BinaryNBT c) _ -> c
      _ -> error ""
{-# INLINE parseChunksDirect1 #-}

parseChunksDirectSchema :: V.Vector ByteString -> V.Vector Chunk
parseChunksDirectSchema = fmap parseChunk
  where
    parseChunk bs = case FP.runParser (get @Chunk) bs of
      FP.OK c _ -> c
      _ -> error ""
{-# INLINE parseChunksDirectSchema #-}

parseChunksDirectSchema1 :: ByteString -> Chunk
parseChunksDirectSchema1 = parseChunk
  where
    parseChunk bs = case FP.runParser (get @Chunk) bs of
      FP.OK c _ -> c
      _ -> error ""
{-# INLINE parseChunksDirectSchema1 #-}

countBlocks :: V.Vector Chunk -> Int
countBlocks = getSum . V.foldMap (\Chunk{sections} -> V.foldMap (\ChunkSection{blocks} -> Sum $ IO.Chunk.countBlocks blocks) sections)
{-# INLINE countBlocks #-}

countBlocks1 :: ChunkSection -> Int
countBlocks1 = (\ChunkSection{blocks} -> IO.Chunk.countBlocks blocks)
{-# INLINE countBlocks1 #-}