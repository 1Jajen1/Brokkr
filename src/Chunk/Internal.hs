module Chunk.Internal (
  Chunk(..)
, ChunkSection(..)
) where

import qualified Data.Vector as V
import Chunk.Position
import Chunk.Section

data Chunk = Chunk {
  position       :: {-# UNPACK #-} !ChunkPosition
, lowestYSection :: {-# UNPACK #-} !Int
, sections       :: {-# UNPACK #-} !(V.Vector ChunkSection)
}
