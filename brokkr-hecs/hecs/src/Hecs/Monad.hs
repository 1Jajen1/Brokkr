{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Hecs.Monad (
  HecsM(..)
, runHecsM
, getWorld
) where

import Hecs.Monad.Class
import qualified Hecs.World as Core

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Base
import Hecs.Filter
import Control.Monad.Trans.Class

newtype HecsM w m a = HecsM { unHecsM :: ReaderT w m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadBase b, MonadBaseControl b)  

runHecsM ::  w -> HecsM w m a -> m a
runHecsM w (HecsM f) = runReaderT f w
{-# INLINE runHecsM #-}

getWorld :: Monad m => HecsM w m w
getWorld = HecsM ask
{-# INLINE getWorld #-}

-- TODO Relax MonadIO to PrimMonad?

instance (MonadBaseControl IO m, Core.WorldClass w) => MonadHecs w (HecsM w m) where
  newEntity = HecsM $ ask >>= liftBase . Core.allocateEntity
  {-# INLINE newEntity #-}
  freeEntity eid = HecsM $ ask >>= liftBase . flip Core.deAllocateEntity eid
  {-# INLINE freeEntity #-}
  addTagWithId eid compId = HecsM ask >>= \w -> Core.addTagWithId w eid compId
  {-# INLINE addTagWithId #-}
  setWithId eid compId comp = HecsM $ ask >>= \w -> liftBase $ Core.setWithId w eid compId comp
  {-# INLINE setWithId #-}
  getWithId eid compId s f = HecsM ask >>= \w -> Core.getWithId w eid compId s f
  {-# INLINE getWithId #-}
  hasTagWithId eid compId = HecsM ask >>= \w -> Core.hasTagWithId w eid compId
  {-# INLINE hasTagWithId #-}
  removeTagWithId eid compId = HecsM ask >>= \w -> Core.removeTagWithId w eid compId
  {-# INLINE removeTagWithId #-}
  removeComponentWithId eid compId = HecsM ask >>= \w -> Core.removeComponentWithId w eid compId
  {-# INLINE removeComponentWithId #-}
  -- hasTagWithId eid compId = HecsM $ ask >>= \w -> liftBase $ Core.getComponentWithId w eid compId (const $ pure True) (pure False)
  filter :: forall b ty . Filter ty HasMainId -> (TypedArchetype ty -> b -> HecsM w m b) -> HecsM w m b -> HecsM w m b
  filter fi f z = HecsM ask >>= \w -> Core.forFilter w fi f z
  {-# INLINE filter #-}
  defer act = do
    a <- HecsM . ReaderT $ \w -> restoreM =<< liftBaseWith (\runInBase -> Core.defer w $ \w' -> runInBase $ runHecsM w' act)
    sync
    pure a
  {-# INLINE defer #-}
  sync = HecsM $ ask >>= \w -> liftBase $ Core.sync w
  {-# INLINE sync #-}
