{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE DataKinds #-}
module Brokkr.Chunk.Position (
  ChunkPosition(..)
, pattern ChunkPosition
) where

import Brokkr.Util.Linear.V2
import Brokkr.Util.Linear.Vector

import Control.DeepSeq

import Data.Hashable

newtype ChunkPosition = ChunkPos (V2 Int)
  deriving stock Show
  deriving newtype (Eq, Ord, NFData, Hashable, VectorSpace Int)

pattern ChunkPosition :: Int -> Int -> ChunkPosition
pattern ChunkPosition x z <- ChunkPos !(V2_Int x z) where
  ChunkPosition !x !z = ChunkPos $ V2_Int x z
{-# COMPLETE ChunkPosition #-}
{-# INLINE CONLIKE ChunkPosition #-}
