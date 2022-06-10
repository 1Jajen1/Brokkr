{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
module IO.Chunkloading (
  Handle(..)
, newFromFolder
) where

import Chunk.Internal
import IO.RegionFile
import qualified FlatParse.Basic as FP
import Util.NBT
import Util.Binary
import Data.IORef
import qualified Data.HashMap.Strict as HM

data Handle = Handle {
  loadChunk :: ChunkPosition -> IO (Maybe Chunk)
}

instance Show Handle where
  show _ = "_"

newFromFolder :: String -> IO Handle
newFromFolder path = do
  -- TODO Implement cache eviction for old files
  rFiles <- newIORef mempty
  let loadChunk cp@(ChunkPos x z) = do
        let rX = x `div` 32
            rZ = z `div` 32
        rMap <- readIORef rFiles
        !rFile <- case HM.lookup (rX, rZ) rMap of
          Just r -> pure r
          Nothing -> do
            !r <- openRegionFile path rX rZ
            writeIORef rFiles (HM.insert (rX, rZ) r rMap)
            pure r
        !mchunkBs <- readChunkData cp rFile
        case mchunkBs of
          Nothing -> error $ "Not generated: " <> show cp
          Just chunkBs ->
            -- TODO Adjust this parser and maybe just store Chunk as Packed 'Chunk instead?
            case FP.runParser (get @(BinaryNBT Chunk)) chunkBs of
              FP.OK (BinaryNBT chunk) _ -> pure $ Just chunk
              _ -> error "TODO" -- TODO
  pure Handle{..}
