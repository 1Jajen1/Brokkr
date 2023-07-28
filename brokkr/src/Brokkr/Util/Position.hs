{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Util.Position (
  Position(..)
, pattern Position
, Falling(..)
) where

import Brokkr.Util.Linear.V3
import Brokkr.Util.Linear.Vector

import Foreign.Storable

import Hecs (Component, ViaBox, ViaFlat)

newtype Position = Pos (V3 Double)
  deriving stock Show
  deriving newtype (Eq, Storable)
  deriving Component via (ViaFlat Position)

deriving newtype instance VectorSpace Double Position

pattern Position :: Double -> Double -> Double -> Position
pattern Position x y z = Pos (V3_Double x y z)
{-# COMPLETE Position #-}

data Falling = OnGround | Falling
  deriving stock (Show, Eq)
  deriving Component via (ViaBox Falling) -- TODO As Tags? If I ever need to iterate only Falling or only OnGround (like in Physics, I may do that)
