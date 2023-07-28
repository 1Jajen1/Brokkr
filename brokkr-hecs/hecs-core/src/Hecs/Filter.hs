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
, getColumn, getColumnWithId
, getEntityColumn
, TypedHas
, iterateArchetype
, And, Or, Not, Tag
, FilterDSL
, filterDSL
, FilterFromList
, HasMain
) where

import Prelude hiding (not)

import Hecs.Filter.Internal hiding (getColumnWithId, iterateArchetype, getEntityColumn)
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

component :: forall c w . (BranchRel c, Component c, Has w c) => Proxy w -> Filter c HasMainId
component w = componentWithId $ getComponentId w
{-# INLINE component #-}

tag :: forall {k} (c :: k) (w :: Type) . (BranchRel c, Has w c) => Proxy w -> Filter c HasMainId
tag w = tagWithId $ getComponentId w
{-# INLINE tag #-}

getColumn :: forall c w ty m . (Component c, Has w c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> m (Column (ComponentKind c) c)
getColumn aty = getColumnWithId @c aty (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE getColumn #-}

getColumnWithId :: forall c ty m . (Component c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> ComponentId c -> m (Column (ComponentKind c) c)
getColumnWithId ty c = liftBase $ Hecs.Filter.Internal.getColumnWithId ty c
{-# INLINE getColumnWithId #-}

iterateArchetype :: MonadBaseControl IO m => TypedArchetype ty -> (Int -> EntityId -> m ()) -> m ()
iterateArchetype ty f = do
  st <- liftBaseWith $ \runInBase -> Hecs.Filter.Internal.iterateArchetype ty (\n eid acc -> runInBase $ restoreM acc >>= \() -> f n eid) (runInBase $ pure ())
  restoreM st
{-# INLINE iterateArchetype #-}

getEntityColumn :: MonadBase IO m => TypedArchetype ty -> m (Column Flat EntityId)
getEntityColumn ty = liftBase $ Hecs.Filter.Internal.getEntityColumn ty
{-# INLINE getEntityColumn #-}

type FilterFromList :: [k] -> k
type family FilterFromList xs where
  FilterFromList (x:y:ys) = And x (FilterFromList (y:ys))
  FilterFromList '[x] = x
  FilterFromList '[] = TypeError ('Text "Cannot create an empty filter (yet)") -- TODO Empty filters could work?

type HasMain :: k -> FilterContext
type family HasMain ty :: FilterContext where
  HasMain (And l r) = CombineCtx (HasMain l) (HasMain r)
  HasMain (Or l r) = CombineCtx (HasMain l) (HasMain r)
  HasMain (Not a) = InvertCtx (HasMain a)
  HasMain (Tag c) = HasMain c
  HasMain c = HasMainId

filterDSL :: forall {k} (w :: Type) (xs :: [k]) . FilterDSL w (FilterFromList xs) => Filter (FilterFromList xs) (HasMain (FilterFromList xs))
filterDSL = filterDSLI (Proxy @w) (Proxy @(FilterFromList xs))
{-# INLINE filterDSL #-}

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
