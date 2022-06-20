module Util.BitSet (
  BitSet
, emptyBitSet
, set
, get
, flip
, complement
, byteSize
) where

import Prelude hiding (flip)

import Data.Word
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Bits hiding (complement)
import qualified Data.Bits
import Util.Binary hiding (get)
import Network.Util.VarNum
import Control.DeepSeq

newtype BitSet = BitSet (Vector Word64)
  deriving newtype (Eq, Show, NFData)

byteSize :: BitSet -> Int
byteSize (BitSet v)
  | V.all (== 0) v = 1
  | otherwise      = varIntSize (V.length v) + 8 * V.length v

instance ToBinary BitSet where
  put (BitSet v)
    | V.all (== 0) v = put (VarInt 0)
    | otherwise = 
       put (VarInt . fromIntegral $ V.length v)
    <> V.foldMap (\x -> put x) v -- TODO Copy directly from memory rather than iterating like this

emptyBitSet :: Int -> BitSet
emptyBitSet len | len < 0 = error "BitSet:emptyBitSet invalid length"
                | otherwise = BitSet $ V.replicate ((len + 63) `div` 64) 0

-- No range checks here! TODO Name these unsafe and offer range checks as a safe api!
set :: BitSet -> Int -> BitSet
set (BitSet v) i =
  let (arrI, wordI) = index i
      w = v V.! arrI
  in BitSet . V.update v $ V.singleton (arrI, w `setBit` wordI)

flip :: BitSet -> Int -> BitSet
flip (BitSet v) i =
  let (arrI, wordI) = index i
      w = v V.! arrI
  in BitSet . V.update v $ V.singleton (arrI, w `xor` (w `setBit` wordI))

get :: BitSet -> Int -> Bool
get (BitSet v) i =
  let (arrI, wordI) = index i
      w = v V.! arrI
  in w `testBit` wordI

complement :: BitSet -> BitSet
complement (BitSet v) = BitSet $ V.map Data.Bits.complement v

index :: Int -> (Int, Int)
index i = (i `unsafeShiftR` 6, i .&. 0b111111)
