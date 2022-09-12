module Command.World (
  Command(..)
) where

import Chunk (Chunk, HasChunkPosition (chunkPosition))
import Optics

data Command =
    CacheLoadedChunk !Chunk

instance Show Command where
  show (CacheLoadedChunk c) = "CacheLoadedChunk " <> show (c ^. chunkPosition)
