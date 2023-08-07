{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Client (
  Client(..)
, Sneaking
, Sprinting
, Flying
) where

import Prelude hiding (init)

import Hecs qualified

newtype Client = Client Hecs.EntityId

-- TODO Don't forget better bitset setting/unsetting so we don't move archetypes
data Sneaking

data Sprinting

-- Creative flying
data Flying

-- TODO Specialize 
