{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Util.Velocity (
  Velocity(..)
) where

import Foreign.Storable

import Util.Binary

import Util.Linear.V3
import Util.Linear.Vector

import Hecs (Component, ViaFlat)

newtype Velocity = Vel (V3 Double)
  deriving stock Show
  deriving newtype (Eq, Storable)
  deriving Component via (ViaFlat Velocity)

deriving newtype instance VectorSpace Double Velocity

pattern Velocity :: Double -> Double -> Double -> Velocity
pattern Velocity x y z = Vel (V3_Double x y z)
{-# COMPLETE Velocity #-}

instance ToBinary Velocity where
  put (Velocity x y z) = put x <> put y <> put z 

instance FromBinary Velocity where
  get = Velocity <$> get <*> get <*> get
