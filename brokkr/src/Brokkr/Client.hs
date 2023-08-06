{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Client (
  Client(..)
, Joined
, Sneaking
, Sprinting
, Flying
) where

import Prelude hiding (init)

import Hecs

newtype Client = Client EntityId

data Joined

-- TODO Don't forget better bitset setting/unsetting so we don't move archetypes
data Sneaking

data Sprinting

-- Creative flying
data Flying

-- TODO Specialize 
