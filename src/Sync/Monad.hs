module Sync.Monad (
  Sync
, runSync
) where

import Data.Functor.Identity
import Data.Coerce

newtype Sync a = Sync (Identity a)
  deriving newtype (Functor, Applicative, Monad)

runSync :: Sync a -> a
runSync = coerce
