{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Hecs.Monad.Class (
  MonadHecs(..)
, add, set, remove
, get, has
, isEnabled, enable, disable
, runFilter_, system_
) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader

import Data.Coerce
import Data.Kind
import Data.Proxy

import Hecs.Component qualified as Core
import Hecs.Entity qualified as Core
import Hecs.Filter qualified as Core
import Hecs.Fold qualified as Core
import Hecs.Query qualified as Core
import Hecs.World qualified as Core

-- TODO Get rid of internal imports, use higher level ones only to avoid relying on hecs-core internals here
-- TODO Consistency between names (hecs <-> hecs-core)

-- | Mtl style typeclass for ecs operations
class Monad m => MonadHecs (w :: Type) (m :: Type -> Type) | m -> w where
  -- | Create a new entity-id.
  -- 
  -- May reuse previously freed entity-id's but that is only observable via internal imports
  newEntity :: m Core.EntityId
  -- | Free an entity-id.
  --
  -- Completely destroys the entity. Removes all associated components and triggers listeners
  freeEntity :: Core.EntityId -> m ()
  -- | Add a component by explicitly passing a component id.
  --
  -- Does not set a value. Reading is dangerous:
  -- - Boxed components will return an unevaluated error
  -- - Flat components will read uninitialized or stale memory
  -- - Tag components are never read and are perfectly fine
  --
  -- See 'add' if you have a statically known component with a 'Has' instance.
  addWithId :: forall c . Core.Component c => Core.EntityId -> Core.ComponentId c -> m ()
  -- | Set a component value with an explicit component id
  --
  -- If a component has not yet been added, the component will be added first.
  --
  -- See 'set' if you have a statically known component with a 'Has' instance
  setWithId :: (Core.Component c, Coercible (Core.ComponentValueFor c) c) => Core.EntityId -> Core.ComponentId c -> c -> m ()
  -- | Get a component value with an explicit component id
  --
  -- Uses cps to help ghc unbox the component if possible.
  --
  -- See 'get' if you have a statically known component with a 'Has' instance
  getWithId :: (Core.Component c, Coercible (Core.ComponentValueFor c) c) => Core.EntityId -> Core.ComponentId c -> (c -> m r) -> m r -> m r
  -- | Check if an entity-id has a component. Uses an explicit component id
  --
  -- See 'has' if you have a statically known component with a 'Has' instance
  hasWithId :: Core.Component c => Core.EntityId -> Core.ComponentId c -> m Bool
  -- | Remove a component with an explicit component id
  --
  -- See 'remove' if you have a statically known component with a 'Has' instance
  removeWithId :: Core.Component c => Core.EntityId -> Core.ComponentId c -> m ()
  -- | Check if a component is enabled, using an explicit component id
  --
  -- See 'isEnabled' if you have a statically known component with a 'Has' instance
  isEnabledWithId :: Core.Component c => Core.EntityId -> Core.ComponentId c -> m Bool
  -- | Enable a component using an explicit component id
  --
  -- Similar to 'addWithId', but does not perform a table move. Degrades iteration
  -- performance as a bitset is now used
  --
  -- See 'enable' if you have a statically known component with a 'Has' instance
  enableWithId :: Core.Component c => Core.EntityId -> Core.ComponentId c -> m ()
  -- | Disable a component using an explicit component id
  --
  -- Similar to 'removeWithId', but does not perform a table move. Degrades iteration
  -- performance as a bitset is now used
  --
  -- See 'disable' if you have a statically known component with a 'Has' instance
  disableWithId :: Core.Component c => Core.EntityId -> Core.ComponentId c -> m ()
  -- | Filter and iterate all archetypes for the given components
  --
  -- This method represents a monadic fold over all matching archetypes
  --
  -- For a convenience filter that directly iterates entities, see 'runFilter_' 
  runFilter :: Core.Filter 'True ty -> (Core.TypedArchetype ty -> b -> m b) -> m b -> m b
  -- | Create a query from a filter
  --
  -- Queries can be run just like filters, but cache the list of matching archetypes.
  -- New archetypes are also automatically matched and inserted.
  query :: Core.Filter 'True ty -> m (Core.Query ty)
  -- | Register a system
  --
  -- Systems are automatically run when 'progress' is called.
  system :: Proxy ins -> Proxy outs -> Core.Filter 'True ty -> (Core.TypedArchetype ty -> b -> m b) -> m b -> (b -> m ()) -> m Core.EntityId
  -- | Progress the world. Argument is delta time from the last call to 'progress'
  --
  -- Runs all enabled systems
  progress :: Int -> m ()

instance MonadHecs w m => MonadHecs w (ReaderT r m) where
  newEntity = lift newEntity
  {-# INLINE newEntity #-}
  freeEntity = lift . freeEntity
  {-# INLINE freeEntity #-}
  addWithId eid cid = lift $ addWithId eid cid
  {-# INLINE addWithId #-}
  setWithId eid cid c = lift $ setWithId eid cid c
  {-# INLINE setWithId #-}
  getWithId eid cid s f = ReaderT $ \r -> getWithId eid cid (\c -> runReaderT (s c) r) (runReaderT f r)
  {-# INLINE getWithId #-}
  hasWithId eid cid = lift $ hasWithId eid cid
  {-# INLINE hasWithId #-}
  removeWithId eid cid = lift $ removeWithId eid cid
  {-# INLINE removeWithId #-}
  isEnabledWithId eid cid = lift $ isEnabledWithId eid cid
  {-# INLINE isEnabledWithId #-}
  enableWithId eid cid = lift $ enableWithId eid cid
  {-# INLINE enableWithId #-}
  disableWithId eid cid = lift $ disableWithId eid cid
  {-# INLINE disableWithId #-}
  runFilter fi s mb = ReaderT $ \r -> Hecs.Monad.Class.runFilter fi (\aty b -> runReaderT (s aty b) r) (runReaderT mb r)
  {-# INLINE runFilter #-}
  query fi = lift $ query fi
  {-# INLINE query #-}
  system ins outs fi f z e = ReaderT $ \r -> system ins outs fi (\aty b -> runReaderT (f aty b) r) (runReaderT z r) (\x -> runReaderT (e x) r)
  {-# INLINE system #-}
  progress = lift . progress
  {-# INLINE progress #-}

-- | Set a component value
--
-- If a component has not yet been added, the component will be added first.
--
-- See 'setWithId' if you need to pass a component id
set :: forall c w m . (MonadHecs w m, Core.Has w c, Core.Component c, Coercible (Core.ComponentValueFor c) c) => Core.EntityId -> c -> m ()
set eid comp = setWithId eid (Core.getComponentId @_ @_ @c (Proxy @w)) comp
{-# INLINE set #-}

-- | Get a component value
--
-- Uses cps to help ghc unbox the component if possible.
--
-- See 'getWithId' if you need to pass a component id
get :: forall c w m r . (MonadHecs w m, Core.Has w c, Core.Component c, Coercible (Core.ComponentValueFor c) c) => Core.EntityId -> (c -> m r) -> m r -> m r
get eid = getWithId eid (Core.getComponentId @_ @_ @c (Proxy @w))
{-# INLINE get #-}

-- | Add a component
--
-- Does not set a value. Reading is dangerous:
-- - Boxed components will return an unevaluated error
-- - Flat components will read uninitialized or stale memory
-- - Tag components are never read and are perfectly fine
--
-- See 'addWithId' if you need to pass a component id
add :: forall c w m . (MonadHecs w m, Core.Component c, Core.Has w c) => Core.EntityId -> m ()
add eid = addWithId @_ @_ @c eid (Core.getComponentId @_ @_ @c (Proxy @w))
{-# INLINE add #-}

-- | Check if an entity-id has a component
--
-- See 'hasWithId' if you need to pass a component id
has :: forall c w m . (MonadHecs w m, Core.Component c, Core.Has w c) => Core.EntityId -> m Bool
has eid = hasWithId @w @m @c eid (Core.getComponentId (Proxy @w))
{-# INLINE has #-}

-- | Remove a component
--
-- See 'removeWithId' if you need to pass a component id
remove :: forall c w m . (MonadHecs w m, Core.Component c, Core.Has w c) => Core.EntityId -> m ()
remove eid = removeWithId @w @m @c eid (Core.getComponentId (Proxy @w))
{-# INLINE remove #-}

-- | Check if a component is enabled
--
-- See 'isEnabledWithId' if you need to pass a component id
isEnabled :: forall c w m . (MonadHecs w m, Core.Component c, Core.Has w c) => Core.EntityId -> m Bool
isEnabled eid = isEnabledWithId @w @m @c eid (Core.getComponentId (Proxy @w))
{-# INLINE isEnabled #-}

-- | Enable a component
--
-- Similar to 'add', but does not perform a table move. Degrades iteration
-- performance as a bitset is now used
--
-- See 'enableWithId' if you need to pass a component id
enable :: forall c w m . (MonadHecs w m, Core.Component c, Core.Has w c) => Core.EntityId -> m ()
enable eid = enableWithId @w @m @c eid (Core.getComponentId (Proxy @w))
{-# INLINE enable #-}

-- | Disable a component
--
-- Similar to 'remove', but does not perform a table move. Degrades iteration
-- performance as a bitset is now used
--
-- See 'disableWithId' if you need to pass a component id
disable :: forall c w m . (MonadHecs w m, Core.Component c, Core.Has w c) => Core.EntityId -> m ()
disable eid = disableWithId @w @m @c eid (Core.getComponentId (Proxy @w))
{-# INLINE disable #-}

-- | Filter and iterate all entities that match the filter
--
-- This method represents a monadic fold over all matching entities
--
-- For a more powerful filter with access to the archetypes see 'runFilter'.
runFilter_ :: forall w ty m a b z
  . ( MonadHecs w m
    , MonadBaseControl IO m
    , Core.FoldBitSets ty
    , Core.HasColumns w ty a, Core.ReadColumns ty a
    , Core.HasColumns w ty b, Core.WriteColumns ty b
    )
  => Core.Filter 'True ty -> Core.FoldM m a b z -> m z
runFilter_ fi fo = Core.toEntityFold @w fo $ \f z e -> runFilter fi (flip f) z >>= e
{-# INLINE runFilter_ #-}

-- | Create a system that iterates all entities that match the filter
--
-- This method represents a monadic fold over all matching entities.
--
-- Note that the monadic state is kept alive only during the fold, but not
-- after. That is: Changes in m, that are not to mutable global state, are
-- not visible after the entire system ran.
--
-- For a more powerful fold with access to the archetypes see 'system'.
system_ :: forall ins outs w ty m a b
  . ( MonadHecs w m
    , MonadBaseControl IO m
    , Core.FoldBitSets ty
    , Core.HasColumns w ty a, Core.ReadColumns ty a
    , Core.HasColumns w ty b, Core.WriteColumns ty b
    )
  => Core.Filter 'True ty -> Core.FoldM m a b () -> m Core.EntityId
system_ fi fo = Core.toEntityFold @w fo $ \f z e -> system (Proxy @ins) (Proxy @outs) fi (flip f) z e
{-# INLINE system_ #-}
