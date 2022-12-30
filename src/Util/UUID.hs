module Util.UUID (
  HasUUID(..)
, module Data.UUID
) where

import Data.UUID

class HasUUID a where
  uuid :: a -> UUID
