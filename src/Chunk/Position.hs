{-# LANGUAGE PatternSynonyms  #-}
module Chunk.Position (
  ChunkPosition
, pattern ChunkPos
) where
import Util.Linear.V2

newtype ChunkPosition = ChunkPosition (V2 Int)

pattern ChunkPos :: Int -> Int -> ChunkPosition
pattern ChunkPos x z <- ChunkPosition !(V2_Int x z) where
  ChunkPos !x !z = ChunkPosition $ V2_Int x z
{-# COMPLETE ChunkPos #-}
{-# INLINE CONLIKE ChunkPos #-}
