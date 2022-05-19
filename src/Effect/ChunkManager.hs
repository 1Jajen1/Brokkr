{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Effect.ChunkManager (
  Handle(loadChunk)
, new
, defaultChunkLoading
) where

import Effectful
import Chunk.Internal
import Effect.IO.File.Effect
import qualified Effect.IO.RegionFile as RegionFile
import qualified FlatParse.Basic as FP
import Util.NBT
import Util.Binary (get)
import Effect.Async

data Handle = Handle {
    loadChunk :: Chunkloading Async
}

type Chunkloading cls = forall es . cls :>> es => ChunkPosition -> Eff es Chunk

new :: Chunkloading Async -> Handle
new loadChunk = Handle{..}

defaultChunkLoading :: forall file . RegionFile.RegionFileFolderPath -> Chunkloading '[File file]
defaultChunkLoading path = \cp@(ChunkPos x z) -> do
  regionFile <- RegionFile.openRegionFile @file path (x `div` 32) (z `div` 32)
  chunkBs <- RegionFile.readChunkData cp regionFile
  case FP.runParser (get @(BinaryNBT Chunk)) chunkBs of
    FP.OK (BinaryNBT chunk) _ -> pure chunk
    _ -> error "TODO" -- TODO
