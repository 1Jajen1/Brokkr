module Sync.Monad (
  Sync
, runSync
) where

import Data.Functor.Identity
import Data.Coerce

-- TODO Make this a writer monad and write into a mutable array list
-- Should lessen gc load
newtype Sync a = Sync (Identity a)
  deriving newtype (Functor, Applicative, Monad)

runSync :: Sync a -> a
runSync = coerce
