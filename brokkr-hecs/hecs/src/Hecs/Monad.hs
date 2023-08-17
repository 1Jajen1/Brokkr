{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Hecs.Monad (
  HecsM(unHecsM)
, pattern HecsM
, runHecsM
, getWorld
) where

import Hecs.Monad.Class
import qualified Hecs.World as Core

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Base
import Hecs.Filter
import Control.Monad.Trans.Class
import GHC.Exts (oneShot)

-- | Concrete monad which implements 'MonadHecs'
newtype HecsM w m a = HecsM_ { unHecsM :: w -> m a }

instance Functor m => Functor (HecsM w m) where
  fmap f (HecsM g) = HecsM (fmap f . g)
  {-# INLINE fmap #-}

instance Monad m => Applicative (HecsM w m) where
  pure a = HecsM (const $ pure a)
  {-# INLINE pure #-}
  ff <*> fa = ff >>= flip fmap fa
  {-# INLINE (<*>) #-}

instance Monad m => Monad (HecsM w m) where
  HecsM f >>= g = HecsM $ \w -> do
    a <- f w
    unHecsM (g a) w
  {-# INLINE (>>=) #-}

instance MonadIO m => MonadIO (HecsM w m) where
  liftIO = HecsM . const . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans (HecsM w) where
  lift = HecsM . const
  {-# INLINE lift #-}

instance MonadTransControl (HecsM w) where
  type StT (HecsM w) a = a
  liftWith act = HecsM $ \w -> act $ runHecsM w
  {-# INLINE liftWith #-}
  restoreT = lift
  {-# INLINE restoreT #-}

instance MonadBase b m => MonadBase b (HecsM w m) where
  liftBase = lift . liftBase
  {-# INLINE liftBase #-}

instance MonadBaseControl b m => MonadBaseControl b (HecsM w m) where
  type StM (HecsM w m) a = StM m a
  liftBaseWith = defaultLiftBaseWith
  {-# INLINE liftBaseWith #-}
  restoreM = defaultRestoreM
  {-# INLINE restoreM #-}

pattern HecsM :: (w -> m a) -> HecsM w m a
pattern HecsM f <- HecsM_ f
  where HecsM f = HecsM_ $ oneShot f
{-# COMPLETE HecsM #-}
{-# INLINE HecsM #-}

-- | Run a 'HecsM' computation by passing in the ecs world
runHecsM ::  w -> HecsM w m a -> m a
runHecsM w (HecsM f) = f w
{-# INLINE runHecsM #-}

-- | Get the current ecs world
getWorld :: Monad m => HecsM w m w
getWorld = HecsM pure
{-# INLINE getWorld #-}

instance (MonadBaseControl IO m, Core.WorldClass w) => MonadHecs w (HecsM w m) where
  newEntity = getWorld >>= liftBase . Core.allocateEntity
  {-# INLINE newEntity #-}
  freeEntity eid = getWorld >>= liftBase . flip Core.deAllocateEntity eid
  {-# INLINE freeEntity #-}
  addTagWithId eid compId = getWorld >>= \w -> Core.addTagWithId w eid compId
  {-# INLINE addTagWithId #-}
  setWithId eid compId comp = getWorld >>= \w -> liftBase $ Core.setWithId w eid compId comp
  {-# INLINE setWithId #-}
  getWithId eid compId s f = getWorld >>= \w -> Core.getWithId w eid compId s f
  {-# INLINE getWithId #-}
  hasTagWithId eid compId = getWorld >>= \w -> Core.hasTagWithId w eid compId
  {-# INLINE hasTagWithId #-}
  removeTagWithId eid compId = getWorld >>= \w -> Core.removeTagWithId w eid compId
  {-# INLINE removeTagWithId #-}
  removeWithId eid compId = getWorld >>= \w -> Core.removeWithId w eid compId
  {-# INLINE removeWithId #-}
  runFilter :: forall b ty . Filter ty HasMainId -> (TypedArchetype ty -> b -> HecsM w m b) -> HecsM w m b -> HecsM w m b
  runFilter fi f z = getWorld >>= \w -> Core.runFilter w fi f z
  {-# INLINE runFilter #-}
  defer act = do
    a <- HecsM $ \w -> restoreM =<< liftBaseWith (\runInBase -> Core.defer w $ \w' -> runInBase $ runHecsM w' act)
    sync
    pure a
  {-# INLINE defer #-}
  sync = getWorld >>= \w -> liftBase $ Core.sync w
  {-# INLINE sync #-}
  registerWithId actionType cid hdl = getWorld >>= \w -> Core.register w actionType cid hdl
  {-# INLINE registerWithId #-}
