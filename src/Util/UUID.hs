module Util.UUID (
  HasUUID(..)
) where

import Data.UUID
import Optics

class HasUUID a where
  uuid :: Getter a UUID
