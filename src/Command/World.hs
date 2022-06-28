module Command.World (
  Command(..)
) where

import Chunk (Chunk)

data Command =
    CacheLoadedChunk !Chunk
  deriving stock Show
