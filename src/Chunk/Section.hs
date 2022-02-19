{-# LANGUAGE DataKinds #-}
module Chunk.Section (
  ChunkSection(..)
, PalettedVector(..)
) where

import Util.Vector.Packed (PackedVector, DynamicNat(..))
import qualified Util.Vector.Packed as P

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import GHC.TypeLits

type NibbleVector = PackedVector ('Static 4096) ('Static 4)

data ChunkSection = ChunkSection {
  y           :: {-# UNPACK #-} !Int
, blockLight  ::                !NibbleVector
, skyLight    ::                !NibbleVector
, blockStates ::                !(PalettedVector SectionSize BlockPaletteMaxBitsize)
, biomes      ::                !(PalettedVector BiomeSectionSize BiomePaletteMaxBitsze)
}

-- TODO Generate at compile time
type BlockPaletteMaxBitsize = 15
type BiomePaletteMaxBitsze  = 6

type SectionSize = 4096
type BiomeSectionSize = 64

data PalettedVector (sz :: Nat) (globalBitSz :: Nat) =
    Global      {-# UNPACK #-} !(PackedVector ('Static sz) ('Static globalBitSz))
  | SingleValue {-# UNPACK #-} !Word
  | Indirect    {-# UNPACK #-} !(Vector Int) {-# UNPACK #-} !(PackedVector ('Static sz) 'Dynamic)

