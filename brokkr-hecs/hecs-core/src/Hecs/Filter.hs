{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
module Hecs.Filter (
  Filter
, FilterContext(HasMainId)
, (.&&.), (.||.)
, not
, component, componentWithId
, tag, tagWithId
, TypedArchetype(..)
, Column(..)
, AccessColumn(..)
, getColumn, getColumnWithId
, getColumnM, getColumnWithIdM
, getEntityColumn
, TypedHas
, iterateArchetype, iterateArchetype_
, And, Or, Not, Tag
, FilterDSL
, filterDSL
, FilterFromList
, HasMain
) where

import Prelude hiding (not)

import Hecs.Filter.Internal hiding (getColumnWithId, iterateArchetype, getEntityColumn, getColumnWithIdM)
import qualified Hecs.Filter.Internal
import Hecs.World.Has
import Hecs.Component.Internal
import Hecs.Component.Relation
import Hecs.Entity.Internal (EntityId)

import Data.Proxy
import GHC.TypeLits
import Data.Kind
import Control.Monad.Base
import Control.Monad.Trans.Control

-- | Create a filter with a static component id
--
-- Rarely used as the TH world generation will generate a method that does not require an explicit type
-- and proxy for 'w'
component :: forall c w . (BranchRel c, Component c, Has w c) => Proxy w -> Filter c HasMainId
component w = componentWithId $ getComponentId w
{-# INLINE component #-}

-- | Create a filter for tags with a static component id
--
-- Rarely used as the TH world generation will generate a method that does not require an explicit type
-- and proxy for 'w'
tag :: forall {k} (c :: k) (w :: Type) . (BranchRel c, Has w c) => Proxy w -> Filter c HasMainId
tag w = tagWithId $ getComponentId w
{-# INLINE tag #-}

-- | Retrieve a component column from a 'TypedArchetype'
--
-- The archetype is guaranteed to have the column
--
-- Rarely used as the TH world generation will generate a method that does not require an explicit type
-- and proxy for 'w'
getColumn :: forall c w ty m . (Component c, Has w c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> m (Column (ComponentKind c) c)
getColumn aty = getColumnWithId @c aty (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE getColumn #-}

-- | Retrieve a component column from a 'TypedArchetype' with an explicit component id
--
-- The archetype is guaranteed to have the column
getColumnWithId :: forall c ty m . (Component c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> ComponentId c -> m (Column (ComponentKind c) c)
getColumnWithId ty c = liftBase $ Hecs.Filter.Internal.getColumnWithId ty c
{-# INLINE getColumnWithId #-}

-- | Retrieve a component column from a 'TypedArchetype'
--
-- Rarely used as the TH world generation will generate a method that does not require an explicit type
-- and proxy for 'w'
getColumnM :: forall c w ty m . (Component c, Has w c, MonadBase IO m) => TypedArchetype ty -> m (Maybe (Column (ComponentKind c) c))
getColumnM aty = getColumnWithIdM @c aty (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE getColumnM #-}


-- | Retrieve a component column from a 'TypedArchetype' with an explicit component id
getColumnWithIdM :: forall c ty m . (Component c, MonadBase IO m) => TypedArchetype ty -> ComponentId c -> m (Maybe (Column (ComponentKind c) c))
getColumnWithIdM aty c = liftBase $ Hecs.Filter.Internal.getColumnWithIdM aty c
{-# INLINE getColumnWithIdM #-}

-- | Iterate all entities currently stored in the passed archetype
--
-- The first argument is the index in the column, the second the entity-id
--
-- Matches a monadic fold structure to allow for many kinds of iteration patterns
iterateArchetype :: MonadBaseControl IO m => TypedArchetype ty -> (Int -> EntityId -> a -> m a) -> m a -> m a
iterateArchetype ty f z = do
  st <- liftBaseWith $ \runInBase -> Hecs.Filter.Internal.iterateArchetype ty (\n eid acc -> runInBase $ restoreM acc >>= f n eid) (runInBase z)
  restoreM st
{-# INLINE iterateArchetype #-}

-- | Iterate all entities currently stored in the passed archetype
--
-- The first argument is the index in the column, the second the entity-id
--
-- Simply iterates, does not accumulate a result other than the changes to monadic context
iterateArchetype_ :: MonadBaseControl IO m => TypedArchetype ty -> (Int -> EntityId -> m ()) -> m ()
iterateArchetype_ ty f = iterateArchetype ty (\n eid _ -> f n eid) (pure ())
{-# INLINE iterateArchetype_ #-}

-- | Explicitly get the column of entity ids stored in the archetype
getEntityColumn :: MonadBase IO m => TypedArchetype ty -> m (Column Flat EntityId)
getEntityColumn ty = liftBase $ Hecs.Filter.Internal.getEntityColumn ty
{-# INLINE getEntityColumn #-}

-- | Transform a list of filter elements into a filter tree 
type FilterFromList :: [k] -> k
type family FilterFromList xs where
  FilterFromList (x:y:ys) = And x (FilterFromList (y:ys))
  FilterFromList '[x] = x
  FilterFromList '[] = TypeError ('Text "Cannot create an empty filter (yet)") -- TODO Empty filters could work?

-- | Keeps track of a filter having at least one main component
type HasMain :: k -> FilterContext
type family HasMain ty :: FilterContext where
  HasMain (And l r) = CombineCtx (HasMain l) (HasMain r)
  HasMain (Or l r) = CombineCtx (HasMain l) (HasMain r)
  HasMain (Not a) = InvertCtx (HasMain a)
  HasMain (Tag c) = HasMain c
  HasMain c = HasMainId

-- | Create a term level filter from a type level filter list
--
-- Rarely used as the TH world generation will generate a method that does not require an explicit type
-- and proxy for 'w'
filterDSL :: forall {k} (w :: Type) (xs :: [k]) . FilterDSL w (FilterFromList xs) => Filter (FilterFromList xs) (HasMain (FilterFromList xs))
filterDSL = filterDSLI (Proxy @w) (Proxy @(FilterFromList xs))
{-# INLINE filterDSL #-}

-- | Typeclass which folds a type level filter into a term level filter
class FilterDSL (w :: Type) (ty :: k) where
  filterDSLI :: Proxy w -> Proxy ty -> Filter ty (HasMain ty)

instance {-# OVERLAPPING #-} FilterDSL w f => FilterDSL w (Not f) where
  filterDSLI p _ = not $ filterDSLI p (Proxy @f)
  {-# INLINE filterDSLI #-}

instance {-# OVERLAPPING #-} (FilterDSL w l, FilterDSL w r) => FilterDSL w (Or l r) where
  filterDSLI p _ = filterDSLI p (Proxy @l) .||. filterDSLI p (Proxy @r) 
  {-# INLINE filterDSLI #-}

instance {-# OVERLAPPING #-} (FilterDSL w l, FilterDSL w r) => FilterDSL w (And l r) where
  filterDSLI p _ = filterDSLI p (Proxy @l) .&&. filterDSLI p (Proxy @r) 
  {-# INLINE filterDSLI #-}

instance {-# OVERLAPPING #-} (Has w (Tag c), HasMain (Tag c) ~ HasMainId) => FilterDSL w (Tag c) where
  filterDSLI p _ = tag p
  {-# INLINE filterDSLI #-}

instance {-# INCOHERENT #-} (BranchRel c, Component c, Has w c, HasMain c ~ HasMainId) => FilterDSL w c where
  filterDSLI p _ = component p
  {-# INLINE filterDSLI #-}
