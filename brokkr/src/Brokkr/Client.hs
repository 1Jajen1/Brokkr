{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Client (
  Client(..)
, Joined
) where

import Prelude hiding (init)

import Hecs

newtype Client = Client EntityId

data Joined

-- TODO Specialize 
