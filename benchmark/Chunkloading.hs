{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PackageImports #-}
module Main (main) where

import Test.Tasty.Bench

import Chunk.Internal
import Network.Packet.Client.Play.ChunkData
import Data.ByteString
import Network.Util.Builder (toStrictSizePrefixedByteString)
import Network.Protocol
import Network.Packet.Client.Play
import Util.Binary
import GHC.Generics
import Control.DeepSeq
import IO.RegionFile
import qualified Data.HashMap.Strict as HM
import Data.Foldable
import Data.Maybe
import Data.List (nub)
import qualified FlatParse.Basic as FP
import "LCraft" Util.NBT
import "LCraft" Util.NBT.Internal
import qualified Data.Vector as V
import qualified IO.Chunkloading as CL

main :: IO ()
main = defaultMain [
    -- TODO Bench the chunkloading handle
    envWithCleanup setupEnv closeEnv $ \ ~(Env{..}) ->
    bgroup "Chunk parsing and sending" $
      [ bench "Chunk -> ChunkData" $ nf mkChunkDataBench chunks
      -- Figure out what is slowing us down here
      -- 1. Buffer allocation?
      -- 2. Actual writes? If so which?
      -- Also test if compression can be sped up by using the c lib directly over the same buffer
      -- compressing in place
      -- Experiment with caching the full packet?
      , bench "ChunkData -> ByteString" $ nf encodeChunkData chunkData
      , bench "IO ByteString" $ nfIO (loadChunkData rFiles toLoad)
      , bench "ByteString -> NBT" $ nf parseNBTs loaded
      , bench "NBT -> Chunk" $ nf parseChunks nbts
      -- write the chunk binary instance to operate on the bytestring directly without an intermediate
      -- NBT parse
      , bench "ByteString -> Chunk" $ nf parseChunksDirect loaded
      ]
  -- TODO This benchmark won't work till I add some way of completion back in
  -- , envWithCleanup setupCLEnv (CL.close . hdl) $ \ ~(CLEnv{..}) -> bench "IO Chunk" . nfIO $ CL.loadChunks hdl toLoad1 (const $ pure ())
  ]

setupCLEnv :: IO CLEnv
setupCLEnv = do
  hdl <- CL.newFromFolder "benchmark/Chunkloading/region"
  let viewDistance = 10
      toLoad1 = V.fromList [ChunkPos x z | x <- [-viewDistance..viewDistance], z <- [-viewDistance..viewDistance]]
  pure CLEnv{..}

data CLEnv = CLEnv {
  toLoad1 :: !(V.Vector ChunkPosition)
, hdl :: CL.Handle
}
  deriving stock Generic
  deriving anyclass NFData

data Env = Env {
  rFiles    :: !(HM.HashMap (Int, Int) RegionFile)
, toLoad    :: !(V.Vector ChunkPosition)
, loaded    :: !(V.Vector ByteString)
, nbts      :: !(V.Vector NBT)
, chunks    :: !(V.Vector Chunk)
, chunkData :: !(V.Vector (Int, ChunkData))
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
      chunkData = mkChunkDataBench chunks
      nbts = parseNBTs loaded
  pure Env{..}

mkChunkDataBench :: V.Vector Chunk -> V.Vector (Int, ChunkData)
mkChunkDataBench = fmap mkChunkData
{-# INLINE mkChunkDataBench #-}

encodeChunkData :: V.Vector (Int, ChunkData) -> V.Vector ByteString
encodeChunkData = fmap $ \(sz, cd) -> toStrictSizePrefixedByteString (Protocol NoCompression NoEncryption) sz (put $ ChunkDataAndUpdateLight cd)
{-# INLINE encodeChunkData #-}

loadChunkData :: HM.HashMap (Int, Int) RegionFile -> V.Vector ChunkPosition -> IO (V.Vector ByteString)
loadChunkData rFiles = traverse loadCData
  where
    loadCData cp =
      let rp = toRfileCoords cp
          Just rFile = HM.lookup rp rFiles
      in fmap fromJust $ readChunkData cp rFile
{-# INLINE loadChunkData #-}

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

parseChunks :: V.Vector NBT -> V.Vector Chunk
parseChunks = fmap (\(NBT _ tag) -> runParser (parseNBT tag) id (error "Woops") tag)
{-# INLINE parseChunks #-}

parseChunksDirect :: V.Vector ByteString -> V.Vector Chunk
parseChunksDirect = fmap parseChunk
  where
    parseChunk bs = case FP.runParser (get @(BinaryNBT Chunk)) bs of
      FP.OK (BinaryNBT c) _ -> c
      _ -> error ""
{-# INLINE parseChunksDirect #-}
