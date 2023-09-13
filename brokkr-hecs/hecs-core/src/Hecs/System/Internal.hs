{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveAnyClass #-}
module Hecs.System.Internal (
  System(..)
, DependsOn
) where

import Data.Bitfield.Internal

import GHC.Exts

import Hecs.Archetype.Internal
import Hecs.Component.Internal
import Hecs.Filter.Internal
import Hecs.World.Has

data System = forall b . System (Archetype# -> ByteArray# -> b -> IO b) (IO b) (b -> IO ())
  deriving Component via ViaBox System

data DependsOn
  deriving anyclass Component

instance Has w System where
  getComponentId _ = ComponentId . EntityId $ Bitfield 3
  {-# INLINE getComponentId #-}

instance Has w DependsOn where
  getComponentId _ = ComponentId . EntityId $ Bitfield 4
  {-# INLINE getComponentId #-}
