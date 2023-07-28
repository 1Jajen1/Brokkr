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
, removeComponent
) where

import qualified Hecs.Entity.Internal as Core
import qualified Hecs.Component as Core
import qualified Hecs.World as Core
import qualified Hecs.World.Has as Core
import qualified Hecs.Filter as Core

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader

import Data.Proxy
import Data.Kind
import Data.Coerce

-- TODO Get rid of internal imports, use higher level ones only to avoid relying on hecs-core internals here
-- TODO Consistency between names (hecs <-> hecs-core)

class Monad m => MonadHecs (w :: Type) (m :: Type -> Type) | m -> w where
  newEntity :: m Core.EntityId
  freeEntity :: Core.EntityId -> m ()
  addTagWithId :: forall {k} c . Core.BranchRel c => Core.EntityId -> Core.ComponentId (c :: k) -> m ()
  setWithId :: (Core.Component c, Core.BranchRel c) => Core.EntityId -> Core.ComponentId c -> c -> m ()
  getWithId :: (Core.Component c, Core.BranchRel c) => Core.EntityId -> Core.ComponentId c -> (c -> m r) -> m r -> m r
  hasTagWithId :: forall {k} (c :: k) . Core.BranchRel c => Core.EntityId -> Core.ComponentId c -> m Bool
  removeTagWithId :: Core.BranchRel c => Core.EntityId -> Core.ComponentId c -> m ()
  removeComponentWithId :: (Core.Component c, Core.BranchRel c) => Core.EntityId -> Core.ComponentId c -> m ()
  filter :: Core.Filter ty Core.HasMainId -> (Core.TypedArchetype ty -> b -> m b) -> m b -> m b
  defer :: m a -> m a
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
  removeComponentWithId eid cid = lift $ removeComponentWithId eid cid
  {-# INLINE removeComponentWithId #-}
  filter fi s mb = ReaderT $ \r -> Hecs.Monad.Class.filter fi (\aty b -> runReaderT (s aty b) r) (runReaderT mb r)
  {-# INLINE filter #-}
  defer act = ReaderT $ \r -> defer $ runReaderT act r
  {-# INLINE defer #-}
  sync = lift sync
  {-# INLINE sync #-}


set :: forall c w m . (MonadHecs w m, Core.BranchRel c, Core.Has w c, Core.Component c) => Core.EntityId -> c -> m ()
set eid comp = setWithId eid (Core.getComponentId @_ @_ @c (Proxy @w)) (coerce comp)
{-# INLINE set #-}

get :: forall c w m r . (MonadHecs w m, Core.BranchRel c, Core.Has w c, Core.Component c) => Core.EntityId -> (c -> m r) -> m r -> m r
get eid = getWithId eid (Core.getComponentId @_ @_ @c (Proxy @w))
{-# INLINE get #-}

addTag :: forall {k} (c :: k) w m . (MonadHecs w m, Core.BranchRel c, Core.BranchRel c, Core.Has w c) => Core.EntityId -> m ()
addTag eid = addTagWithId @_ @_ @c eid (Core.getComponentId @_ @_ @c (Proxy @w))
{-# INLINE addTag #-}

hasTag :: forall c w m . (MonadHecs w m, Core.BranchRel c, Core.Has w c) => Core.EntityId -> m Bool
hasTag eid = hasTagWithId @w @m @c eid (Core.getComponentId (Proxy @w))
{-# INLINE hasTag #-}

removeTag :: forall c w m . (MonadHecs w m, Core.BranchRel c, Core.Has w c) => Core.EntityId -> m ()
removeTag eid = removeTagWithId @w @m @c eid (Core.getComponentId (Proxy @w))
{-# INLINE removeTag #-}

removeComponent :: forall c w m . (MonadHecs w m, Core.BranchRel c, Core.Component c, Core.Has w c) => Core.EntityId -> m ()
removeComponent eid = removeComponentWithId @w @m @c eid (Core.getComponentId (Proxy @w))
{-# INLINE removeComponent #-}
