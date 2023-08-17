{-# LANGUAGE AllowAmbiguousTypes #-}
module Hecs.World (
  Has
, allocateEntity
, deAllocateEntity
, get, getWithId
, hasTag, hasTagWithId
, addTag, addTagWithId
, set, setWithId
, removeTag, removeTagWithId
, remove, removeWithId
, WorldClass
, WorldImpl
, runFilter, runFilter_
, defer
, sync
, register
, ActionType(..)
) where

import Prelude hiding (filter)

import Hecs.Entity.Internal (EntityId)
import Hecs.Component

import Hecs.World.Has
import Hecs.World.Internal
import Hecs.Filter

import Control.Monad (void)
import Control.Monad.Base
import Control.Monad.Trans.Control

import Data.Proxy
import Hecs.Fold

get :: forall c w r m . (WorldClass w, BranchRel c, Component c, Has w c, MonadBaseControl IO m) => w -> EntityId -> (c -> m r) -> m r -> m r
get w eid = getWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE get #-}

getWithId :: forall c w r m . (WorldClass w, BranchRel c, Component c, MonadBaseControl IO m) => w -> EntityId -> ComponentId c -> (c -> m r) -> m r -> m r
getWithId w eid cid s f = do
  st <- liftBaseWith $ \runInBase -> getI w eid cid (runInBase . s) (runInBase f)
  restoreM st 
{-# INLINE getWithId #-}

hasTag :: forall c w m . (WorldClass w, BranchRel c, Has w c, MonadBase IO m) => w -> EntityId -> m Bool
hasTag w eid = hasTagWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE hasTag #-}

hasTagWithId :: forall c w m . (WorldClass w, BranchRel c, MonadBase IO m) => w -> EntityId -> ComponentId c -> m Bool
hasTagWithId w eid compId = liftBase $ hasTagI w eid compId
{-# INLINE hasTagWithId #-}

addTag :: forall c w m . (WorldClass w, BranchRel c, Has w c, MonadBase IO m) => w -> EntityId -> m ()
addTag w eid = addTagWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE addTag #-}

addTagWithId :: forall c w m . (WorldClass w, BranchRel c, MonadBase IO m) => w -> EntityId -> ComponentId c -> m ()
addTagWithId w eid compId = liftBase $ addTagI w eid compId
{-# INLINE addTagWithId #-}

set :: forall c w m . (WorldClass w, BranchRel c, Component c, Has w c, MonadBase IO m) => w -> EntityId -> c -> m ()
set w eid = setWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE set #-}

setWithId :: forall c w m . (WorldClass w, BranchRel c, Component c, MonadBase IO m) => w -> EntityId -> ComponentId c -> c -> m ()
setWithId w eid cid c = liftBase $ setI w eid cid c
{-# INLINE setWithId #-}

removeTag :: forall c w m . (WorldClass w, BranchRel c, Has w c, MonadBase IO m) => w -> EntityId -> m ()
removeTag w eid = removeTagWithId w eid (getComponentId @_ @_ @c (Proxy @w)) 
{-# INLINE removeTag #-}

removeTagWithId :: forall c w m . (WorldClass w, BranchRel c, MonadBase IO m) => w -> EntityId -> ComponentId c -> m ()
removeTagWithId w eid cid = liftBase $ removeTagI w eid cid
{-# INLINE removeTagWithId #-}

remove :: forall c w m . (WorldClass w, BranchRel c, Component c, Has w c, MonadBase IO m) => w -> EntityId -> m ()
remove w eid = removeWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE remove #-}

removeWithId :: forall c w m . (WorldClass w, BranchRel c, Component c, MonadBase IO m) => w -> EntityId -> ComponentId c -> m ()
removeWithId w eid cid = liftBase $ removeComponentI w eid cid
{-# INLINE removeWithId #-}

runFilter :: (WorldClass w, MonadBaseControl IO m) => w -> Filter ty HasMainId -> (TypedArchetype ty -> b -> m b) -> m b -> m b
runFilter w fi f z = do
  st <- liftBaseWith $ \runInBase -> filterI w fi (\aty b -> runInBase $ restoreM b >>= f aty) (runInBase z)
  restoreM st
{-# INLINE runFilter #-}

runFilter_ :: forall w ty m a b z
  . ( WorldClass w
    , MonadBaseControl IO m
    , HasColumns w ty a, ReadColumns ty a
    , HasColumns w ty b, WriteColumns ty b
    )
  => w -> Filter ty HasMainId -> FoldM m a b z -> m z
runFilter_ w fi fo = toEntityFold @w fo $ \f z e -> runFilter w fi (flip f) z >>= e
{-# INLINE runFilter_ #-}

register :: (WorldClass w, MonadBaseControl IO m) => w -> ActionType -> ComponentId c -> (EntityId -> m ()) -> m ()
register w ty cid hdl = liftBaseWith $ \runInBase -> registerI w ty cid (void . runInBase . hdl)
{-# INLINE register #-}
