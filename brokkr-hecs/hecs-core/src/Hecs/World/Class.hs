{-# LANGUAGE AllowAmbiguousTypes #-}
module Hecs.World.Class (
 WorldOps(..)
, has, get
, add, set, remove
, isEnabled, enable, disable
, runFilter_
) where

import Control.Monad.Base
import Control.Monad.Trans.Control

import Data.Coerce
import Data.Proxy

import Hecs.Component
import Hecs.Entity
import Hecs.Filter
import Hecs.Fold
import Hecs.Query
import Hecs.World.Has

class WorldOps w where
  newEntity  :: MonadBase IO m => w -> m EntityId
  freeEntity :: MonadBase IO m => w -> EntityId -> m ()
  hasWithId :: (MonadBase IO m, Component c) => w -> EntityId -> ComponentId c -> m Bool
  getWithId :: (MonadBaseControl IO m, Component c, Coercible (ComponentValueFor c) c) => w -> EntityId -> ComponentId c -> (c -> m r) -> m r -> m r
  addWithId :: (MonadBase IO m, Component c) => w -> EntityId -> ComponentId c -> m ()
  setWithId :: (MonadBase IO m, Component c, Coercible (ComponentValueFor c) c) => w -> EntityId -> ComponentId c -> c -> m ()
  removeWithId :: (MonadBase IO m, Component c) => w -> EntityId -> ComponentId c -> m ()
  isEnabledWithId :: (MonadBase IO m, Component c) => w -> EntityId -> ComponentId c -> m Bool
  enableWithId :: (MonadBase IO m, Component c) => w -> EntityId -> ComponentId c -> m ()
  disableWithId :: (MonadBase IO m, Component c) => w -> EntityId -> ComponentId c -> m ()
  runFilter :: MonadBaseControl IO m => w -> Filter 'True ty -> (TypedArchetype ty -> b -> m b) -> m b -> m b
  query :: MonadBase IO m => w -> Filter 'True ty -> m (Query ty)
  system :: MonadBaseControl IO m => Proxy ins -> Proxy outs -> w -> Filter 'True ty -> (TypedArchetype ty -> b -> m b) -> m b -> (b -> m ()) -> m EntityId
  progress :: MonadBase IO m => w -> Int -> m ()

has :: forall w c m . (WorldOps w, Has w c, MonadBase IO m, Component c) => w -> EntityId -> m Bool
{-# INLINE has #-}
has w eid = hasWithId w eid (getComponentId @_ @_ @c (Proxy @w))

get :: forall w c m r . (WorldOps w, Has w c, MonadBaseControl IO m, Component c, Coercible (ComponentValueFor c) c) => w -> EntityId -> (c -> m r) -> m r -> m r
{-# INLINE get #-}
get w eid = getWithId w eid (getComponentId @_ @_ @c (Proxy @w))

add :: forall w c m . (WorldOps w, Has w c, MonadBase IO m, Component c) => w -> EntityId -> m ()
{-# INLINE add #-}
add w eid = addWithId w eid (getComponentId @_ @_ @c (Proxy @w))

set :: forall w c m . (WorldOps w, Has w c, MonadBase IO m, Component c, Coercible (ComponentValueFor c) c) => w -> EntityId -> c -> m ()
{-# INLINE set #-}
set w eid c = setWithId w eid (getComponentId @_ @_ @c (Proxy @w)) c

remove :: forall w c m . (WorldOps w, Has w c, MonadBase IO m, Component c) => w -> EntityId -> m ()
{-# INLINE remove #-}
remove w eid = removeWithId w eid (getComponentId @_ @_ @c (Proxy @w))

isEnabled :: forall w c m . (WorldOps w, Has w c, MonadBase IO m, Component c) => w -> EntityId -> m Bool
{-# INLINE isEnabled #-}
isEnabled w eid = isEnabledWithId w eid (getComponentId @_ @_ @c (Proxy @w))

enable :: forall w c m . (WorldOps w, Has w c, MonadBase IO m, Component c) => w -> EntityId -> m ()
{-# INLINE enable #-}
enable w eid = enableWithId w eid (getComponentId @_ @_ @c (Proxy @w))

disable :: forall w c m . (WorldOps w, Has w c, MonadBase IO m, Component c) => w -> EntityId -> m ()
{-# INLINE disable #-}
disable w eid = disableWithId w eid (getComponentId @_ @_ @c (Proxy @w))

runFilter_
  :: forall w ty a b m z
  . ( WorldOps w
    , MonadBaseControl IO m
    , FoldBitSets ty
    , HasColumns w ty a, ReadColumns ty a
    , HasColumns w ty b, WriteColumns ty b
    )
  => w
  -> Filter 'True ty
  -> FoldM m a b z
  -> m z
{-# INLINE runFilter_ #-}
runFilter_ w fi fo = toEntityFold @w fo $ \f z e -> runFilter w fi (flip f) z >>= e

-- TODO Docs: The fold here and the one passed up in system has two oddities compared to queries/filters
--  - it requires the extract step to map to unit, ie there is no final result (but there can be intermediates)
--  - the monadic state is thrown away. As such actions like writes to pure monadic state do not persist between
--    system invocations. They do however persist for the entire fold of *one* run.

system_
  :: forall w ty ins outs a b m
  . ( WorldOps w
    , MonadBaseControl IO m
    , FoldBitSets ty
    , HasColumns w ty a, ReadColumns ty a
    , HasColumns w ty b, WriteColumns ty b
    )
  => Proxy ins -> Proxy outs
  -> w
  -> Filter 'True ty
  -> FoldM m a b ()
  -> m EntityId
{-# INLINE system_ #-}
system_ ins outs w fi fo = toEntityFold @w fo $ \f z e -> system ins outs w fi (flip f) z e
