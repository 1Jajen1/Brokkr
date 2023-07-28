{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Util.PalettedVector (
  PalettedVector(..)
, unsafeIndex
) where

import Brokkr.PackedVector (DynamicNat(..), PackedVector)
import Brokkr.PackedVector qualified as P

import Control.DeepSeq

import Data.Vector.Storable qualified as S

import GHC.TypeLits

data PalettedVector (sz :: Nat) (globalBitSz :: Nat) =
    Global      {-# UNPACK #-} !(PackedVector ('Static sz) ('Static globalBitSz) Int)
  | SingleValue {-# UNPACK #-} !Int
  -- TODO Use Unboxed vectors instead. They should be better for such small vectors
  | Indirect    {-# UNPACK #-} !(S.Vector Int) {-# UNPACK #-} !(PackedVector ('Static sz) 'Dynamic Int)
  deriving stock Eq

unsafeIndex :: (KnownNat sz, KnownNat bsz) => Int -> PalettedVector sz bsz -> Int
unsafeIndex _ (SingleValue x) = x
unsafeIndex i (Global pv) = P.unsafeIndex pv i
unsafeIndex i (Indirect palette pv) =
  let el = P.unsafeIndex pv i
  in S.unsafeIndex palette el

instance (KnownNat sz, KnownNat mBSz, KnownNat (Div 64 mBSz)) => Show (PalettedVector sz mBSz) where
  show (Global v)      = "Global " <> show v
  show (SingleValue v) = "Single " <> show v
  show (Indirect p v)  = "Indirect " <> show p <> show v 

instance NFData (PalettedVector sz maxBitSz) where
  rnf (Global vec) = rnf vec
  rnf (SingleValue w) = rnf w
  rnf (Indirect p vec) = rnf p `seq` rnf vec
