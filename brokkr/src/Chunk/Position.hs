{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE DataKinds #-}
module Chunk.Position (
  ChunkPosition
, pattern ChunkPos
) where

import Util.Linear.Vector
import Util.Linear.V2
import Control.DeepSeq
import Data.Hashable

newtype ChunkPosition = ChunkPosition (V2 Int)
  deriving stock Show
  deriving newtype (Eq, Ord, NFData, Hashable, VectorSpace Int)

pattern ChunkPos :: Int -> Int -> ChunkPosition
pattern ChunkPos x z <- ChunkPosition !(V2_Int x z) where
  ChunkPos !x !z = ChunkPosition $ V2_Int x z
{-# COMPLETE ChunkPos #-}
{-# INLINE CONLIKE ChunkPos #-}
