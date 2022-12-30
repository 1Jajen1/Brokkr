{-# LANGUAGE DeriveAnyClass #-}
module Chunk.Section (
  ChunkSection
, BlockStates(..)
, Biomes(..)
) where

import Util.Vector.Packed (PackedVector, DynamicNat(..))

import GHC.TypeLits
import Block.Internal.Conversion
import GHC.Generics (Generic)
import Control.DeepSeq
import Util.Binary
import Util.PalettedVector

type NibbleVector = PackedVector ('Static 4096) ('Static 4)

-- TODO Try and compress blocks/light by using octtrees, maybe just if the palette is small as a heuristic (compress them, calc byte size and compare to arr version)
-- if its smaller use the octtree otherwise stick with the array. 
data ChunkSection = ChunkSection {
  y          :: {-# UNPACK #-} !Int
, blockLight ::                !(Maybe NibbleVector)
, skyLight   ::                !(Maybe NibbleVector)
, blocks     ::                !BlockStates
, biomes     ::                !Biomes
, blockCount :: {-# UNPACK #-} !Int
}
  deriving stock (Show, Generic)
  deriving anyclass NFData

-- TODO Generate at compile time since 1 + Log2 is not actually correct if the HighestBlockStateId is a perfect power of 2 
type BlockPaletteMaxBitsize = 1 + Log2 HighestBlockStateId 
type BiomePaletteMaxBitsze  = 6

type SectionSize = 4096
type BiomeSectionSize = 64

newtype BlockStates = BlockStates (PalettedVector SectionSize BlockPaletteMaxBitsize)
  deriving stock Show
  deriving newtype (NFData, ToBinary)

newtype Biomes = Biomes (PalettedVector BiomeSectionSize BiomePaletteMaxBitsze)
  deriving stock Show
  deriving newtype (NFData, ToBinary)
