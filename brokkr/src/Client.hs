{-# LANGUAGE UndecidableInstances #-}
module Client (
  Client(..)
, Joined
) where

import Prelude hiding (init)

import Hecs

newtype Client = Client EntityId

data Joined

-- TODO Specialize 
