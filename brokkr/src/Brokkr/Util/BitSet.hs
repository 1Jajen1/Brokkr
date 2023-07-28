module Brokkr.Util.BitSet (
  BitSet(..)
, emptyBitSet
, set
, get
, flip
, complement
) where

import Prelude hiding (flip)

import Control.DeepSeq

import Data.Bits hiding (complement)
import Data.Bits qualified

import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V

import Data.Word

-- TODO do a lot more here
-- TODO Mutable version
newtype BitSet = BitSet (Vector Word64)
  deriving newtype (Eq, Show, NFData)

emptyBitSet :: Int -> BitSet
emptyBitSet len | len < 0 = error "BitSet:emptyBitSet invalid length"
                | otherwise = BitSet $ V.replicate ((len + 63) `div` 64) 0

-- No range checks here! TODO Name these unsafe and offer range checks as a safe api!
set :: BitSet -> Int -> BitSet
set (BitSet v) i =
  let (arrI, wordI) = index i
      w = v V.! arrI
  in BitSet $ V.update_ v (V.singleton arrI) (V.singleton (w `setBit` wordI))

flip :: BitSet -> Int -> BitSet
flip (BitSet v) i =
  let (arrI, wordI) = index i
      w = v V.! arrI
  in BitSet $ V.update_ v (V.singleton arrI) (V.singleton (w `xor` (w `setBit` wordI)))

get :: BitSet -> Int -> Bool
get (BitSet v) i =
  let (arrI, wordI) = index i
      w = v V.! arrI
  in w `testBit` wordI

complement :: BitSet -> BitSet
complement (BitSet v) = BitSet $ V.map Data.Bits.complement v

index :: Int -> (Int, Int)
index i = (i `unsafeShiftR` 6, i .&. 0b111111)
