{-# LANGUAGE UndecidableInstances #-}
module Client (
  Client(..)
, Joined
) where

import Prelude hiding (init)

import Monad

import GHC.Generics

import Network.Connection (Connection)
import Util.UUID

import Hecs

newtype Client = Client EntityId

data Joined
  deriving Component via (ViaTag Joined)

-- TODO Specialize 
