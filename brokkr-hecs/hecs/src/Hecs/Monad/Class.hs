{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Hecs.Monad.Class (
  MonadHecs(..)
, set
, get
, addTag
, hasTag
, removeTag
, remove
, runFilter_
) where

import qualified Hecs.Entity.Internal as Core
import qualified Hecs.Component as Core
import qualified Hecs.World as Core
import qualified Hecs.World.Has as Core
import qualified Hecs.Filter as Core
import qualified Hecs.Fold as Core

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader

import Data.Proxy
import Data.Kind
import Data.Coerce

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
  -- | Add a tag component by explicitly passing a component id
  --
  -- See 'addTag' if you have a statically known tag with a 'Has' instance.
  addTagWithId :: forall {k} c . Core.BranchRel c => Core.EntityId -> Core.ComponentId (c :: k) -> m ()
  -- | Set a component value with an explicit component id
  --
  -- If a component has not yet been added, the component will be added first.
  --
  -- See 'set' if you have a statically known tag with a 'Has' instance
  setWithId :: (Core.Component c, Core.BranchRel c) => Core.EntityId -> Core.ComponentId c -> c -> m ()
  -- | Get a component value with an explicit component id
  --
  -- Uses cps to help ghc unbox the component if possible.
  --
  -- See 'get' if you have a statically known tag with a 'Has' instance
  getWithId :: (Core.Component c, Core.BranchRel c) => Core.EntityId -> Core.ComponentId c -> (c -> m r) -> m r -> m r
  -- | Check if an entity-id has a tag component. Uses an explicit component id
  --
  -- See 'hasTag' if you have a statically known tag with a 'Has' instance
  hasTagWithId :: forall {k} (c :: k) . Core.BranchRel c => Core.EntityId -> Core.ComponentId c -> m Bool
  -- | Remove a tag component with an explicit component id
  --
  -- See 'removeTag' if you have a statically known tag with a 'Has' instance
  removeTagWithId :: Core.BranchRel c => Core.EntityId -> Core.ComponentId c -> m ()
  -- | Remove a component with an explicit component id
  --
  -- See 'remove' if you have a statically known tag with a 'Has' instance
  removeWithId :: (Core.Component c, Core.BranchRel c) => Core.EntityId -> Core.ComponentId c -> m ()
  -- | Filter and iterate all archetypes for the given components
  --
  -- This method represents a monadic fold over all matching archetypes
  --
  -- For a convenience filter that directly iterates entities, see 'runFilter_' 
  runFilter :: Core.Filter ty Core.HasMainId -> (Core.TypedArchetype ty -> b -> m b) -> m b -> m b
  -- | Defer most updates to the world
  --
  -- Updates are written to a shared queue and processed after either the passed computation ends
  -- or 'sync' is explicitly called
  defer :: m a -> m a
  -- | Apply all deferred updates
  --
  -- 'defer' will generally apply updates on its own, but in some contexts, for example if
  -- the argument to 'defer' diverges (runs in an infinite loop in some network thread for example)
  -- it is useful to manually sync changes.
  sync :: m ()

instance MonadHecs w m => MonadHecs w (ReaderT r m) where
  newEntity = lift newEntity
  {-# INLINE newEntity #-}
  freeEntity = lift . freeEntity
  {-# INLINE freeEntity #-}
  addTagWithId eid cid = lift $ addTagWithId eid cid
  {-# INLINE addTagWithId #-}
  setWithId eid cid c = lift $ setWithId eid cid c
  {-# INLINE setWithId #-}
  getWithId eid cid s f = ReaderT $ \r -> getWithId eid cid (\c -> runReaderT (s c) r) (runReaderT f r)
  {-# INLINE getWithId #-}
  hasTagWithId eid cid = lift $ hasTagWithId eid cid
  {-# INLINE hasTagWithId #-}
  removeTagWithId eid cid = lift $ removeTagWithId eid cid
  {-# INLINE removeTagWithId #-}
  removeWithId eid cid = lift $ removeWithId eid cid
  {-# INLINE removeWithId #-}
  runFilter fi s mb = ReaderT $ \r -> Hecs.Monad.Class.runFilter fi (\aty b -> runReaderT (s aty b) r) (runReaderT mb r)
  {-# INLINE runFilter #-}
  defer act = ReaderT $ \r -> defer $ runReaderT act r
  {-# INLINE defer #-}
  sync = lift sync
  {-# INLINE sync #-}

-- | Set a component value
--
-- If a component has not yet been added, the component will be added first.
--
-- See 'setWithId' if you need to pass a component id
set :: forall c w m . (MonadHecs w m, Core.BranchRel c, Core.Has w c, Core.Component c) => Core.EntityId -> c -> m ()
set eid comp = setWithId eid (Core.getComponentId @_ @_ @c (Proxy @w)) (coerce comp)
{-# INLINE set #-}

-- | Get a component value
--
-- Uses cps to help ghc unbox the component if possible.
--
-- See 'getWithId' if you need to pass a component id
get :: forall c w m r . (MonadHecs w m, Core.BranchRel c, Core.Has w c, Core.Component c) => Core.EntityId -> (c -> m r) -> m r -> m r
get eid = getWithId eid (Core.getComponentId @_ @_ @c (Proxy @w))
{-# INLINE get #-}

-- | Add a tag component
--
-- See 'addTagWithId' if you need to pass a component id
addTag :: forall {k} (c :: k) w m . (MonadHecs w m, Core.BranchRel c, Core.BranchRel c, Core.Has w c) => Core.EntityId -> m ()
addTag eid = addTagWithId @_ @_ @c eid (Core.getComponentId @_ @_ @c (Proxy @w))
{-# INLINE addTag #-}

-- | Check if an entity-id has a tag component
--
-- See 'hasTagWithId' if you need to pass a component id
hasTag :: forall c w m . (MonadHecs w m, Core.BranchRel c, Core.Has w c) => Core.EntityId -> m Bool
hasTag eid = hasTagWithId @w @m @c eid (Core.getComponentId (Proxy @w))
{-# INLINE hasTag #-}

-- | Remove a tag component
--
-- See 'removeTagWithId' if you need to pass a component id
removeTag :: forall c w m . (MonadHecs w m, Core.BranchRel c, Core.Has w c) => Core.EntityId -> m ()
removeTag eid = removeTagWithId @w @m @c eid (Core.getComponentId (Proxy @w))
{-# INLINE removeTag #-}

-- | Remove a component
--
-- See 'removeWithId' if you need to pass a component id
remove :: forall c w m . (MonadHecs w m, Core.BranchRel c, Core.Component c, Core.Has w c) => Core.EntityId -> m ()
remove eid = removeWithId @w @m @c eid (Core.getComponentId (Proxy @w))
{-# INLINE remove #-}

-- | Filter and iterate all entities that match the filter
  --
  -- This method represents a monadic fold over all matching entities
  --
  -- For a more powerful filter with access to the archetypes see 'runFilter'.
runFilter_ :: forall w ty m a b z
  . ( MonadHecs w m
    , MonadBaseControl IO m
    , Core.HasColumns w ty a, Core.ReadColumns ty a
    , Core.HasColumns w ty b, Core.WriteColumns ty b
    )
  => Core.Filter ty Core.HasMainId -> Core.FoldM m a b z -> m z
runFilter_ fi fo = Core.toEntityFold @w fo $ \f z e -> runFilter fi (flip f) z >>= e
{-# INLINE runFilter_ #-}
