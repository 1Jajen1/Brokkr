{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Chunk.Internal (
  Chunk(..)
, ChunkSection(..)
, ChunkPosition
, pattern ChunkPos
, Heightmaps
) where

import qualified Data.Vector as V
import Chunk.Position
import Chunk.Section
import Util.NBT
import Data.Int
import GHC.Generics (Generic)
import Control.DeepSeq
import Util.Vector.Packed
import Optics

import Chunk.Heightmap

numSections :: Int
numSections = 26 -- TODO Derive from world height value

data Chunk = Chunk {
  _position       :: {-# UNPACK #-} !ChunkPosition
, _lowestYSection :: {-# UNPACK #-} !Int
, _sections       :: {-# UNPACK #-} !(V.Vector ChunkSection)
, _heightmaps     :: {-# UNPACK #-} !Heightmaps
}
  deriving stock (Show, Generic)
  deriving anyclass NFData

--
instance HasChunkPosition Chunk where
  chunkPosition = to _position
  {-# INLINE chunkPosition #-}
