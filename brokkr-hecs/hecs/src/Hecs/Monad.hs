{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Hecs.Monad (
  HecsM(unHecsM)
, pattern HecsM
, runHecsM
, getWorld
) where


import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control

import GHC.Exts (oneShot)

import Hecs.Filter
import Hecs.Monad.Class
import Hecs.World qualified as Core

-- | Concrete monad transfomer which implements 'MonadHecs'
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

instance PrimMonad m => PrimMonad (HecsM w m) where
  type PrimState (HecsM w m) = PrimState m
  primitive f = HecsM $ \_ -> primitive f
  {-# INLINE primitive #-}

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

instance (MonadBaseControl IO m, Core.WorldOps w) => MonadHecs w (HecsM w m) where
  newEntity = getWorld >>= liftBase . Core.newEntity
  {-# INLINE newEntity #-}
  freeEntity eid = getWorld >>= liftBase . flip Core.freeEntity eid
  {-# INLINE freeEntity #-}
  addWithId eid compId = getWorld >>= \w -> Core.addWithId w eid compId
  {-# INLINE addWithId #-}
  setWithId eid compId comp = getWorld >>= \w -> liftBase $ Core.setWithId w eid compId comp
  {-# INLINE setWithId #-}
  getWithId eid compId s f = getWorld >>= \w -> Core.getWithId w eid compId s f
  {-# INLINE getWithId #-}
  hasWithId eid compId = getWorld >>= \w -> Core.hasWithId w eid compId
  {-# INLINE hasWithId #-}
  removeWithId eid compId = getWorld >>= \w -> Core.removeWithId w eid compId
  {-# INLINE removeWithId #-}
  isEnabledWithId eid compId = getWorld >>= \w -> Core.isEnabledWithId w eid compId
  {-# INLINE isEnabledWithId #-}
  enableWithId eid compId = getWorld >>= \w -> Core.enableWithId w eid compId
  {-# INLINE enableWithId #-}
  disableWithId eid compId = getWorld >>= \w -> Core.disableWithId w eid compId
  {-# INLINE disableWithId #-}
  runFilter :: forall b ty . Filter 'True ty -> (TypedArchetype ty -> b -> HecsM w m b) -> HecsM w m b -> HecsM w m b
  runFilter fi f z = getWorld >>= \w -> Core.runFilter w fi f z
  {-# INLINE runFilter #-}
  query fi = getWorld >>= \w -> Core.query w fi
  {-# INLINE query #-}
  system ins outs fi f z e = getWorld >>= \w -> Core.system ins outs w fi f z e
  {-# INLINE system #-}
  progress dt = getWorld >>= \w -> Core.progress w dt
  {-# INLINE progress #-}
