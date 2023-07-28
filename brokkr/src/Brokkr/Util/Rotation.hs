{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Util.Rotation (
  Rotation(..)
, pattern Rotation
) where

import Brokkr.Util.Linear.V2
import Brokkr.Util.Linear.Vector

import Foreign.Storable

import Hecs (Component, ViaFlat)

newtype Rotation = Rot (V2 Float)
  deriving stock Show
  deriving newtype (Eq, Storable)
  deriving Component via (ViaFlat Rotation)

deriving newtype instance VectorSpace Float Rotation

pattern Rotation :: Float -> Float -> Rotation
pattern Rotation yaw pitch = Rot (V2_Float yaw pitch)
{-# COMPLETE Rotation #-}
