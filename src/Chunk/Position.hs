{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Chunk.Position (
  ChunkPosition
, pattern ChunkPos
, HasChunkPosition(..)
) where
import Util.Linear.V2
import Control.DeepSeq
import Data.Hashable
import Optics

newtype ChunkPosition = ChunkPosition (V2 Int)
  deriving stock Show
  deriving newtype (Eq, NFData, Hashable)

pattern ChunkPos :: Int -> Int -> ChunkPosition
pattern ChunkPos x z <- ChunkPosition !(V2_Int x z) where
  ChunkPos !x !z = ChunkPosition $ V2_Int x z
{-# COMPLETE ChunkPos #-}
{-# INLINE CONLIKE ChunkPos #-}

class HasChunkPosition a where
  chunkPosition :: Getter a ChunkPosition
