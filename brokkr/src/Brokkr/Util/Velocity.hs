{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Util.Velocity (
  Velocity(..)
, pattern Velocity
) where

import Brokkr.Util.Linear.V3
import Brokkr.Util.Linear.Vector

import Foreign.Storable

import Hecs (Component, ViaFlat)

newtype Velocity = Vel (V3 Double)
  deriving stock Show
  deriving newtype (Eq, Storable)
  deriving Component via (ViaFlat Velocity)

deriving newtype instance VectorSpace Double Velocity

pattern Velocity :: Double -> Double -> Double -> Velocity
pattern Velocity x y z = Vel (V3_Double x y z)
{-# COMPLETE Velocity #-}
