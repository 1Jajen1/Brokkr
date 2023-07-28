{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Hecs.Filter.Internal (
  Filter(..)
, FilterContext(..)
, CombineCtx
, InvertCtx
, extractMainId
, evaluate
, (.&&.), (.||.)
, not
, componentWithId
, tagWithId
, TypedArchetype(..)
, getColumnWithId
, iterateArchetype
, TypedHas
, getEntityColumn
, And, Or, Not, Tag
) where

import Prelude hiding (not)
import qualified Prelude

import Hecs.Archetype.Internal hiding (empty)
import Hecs.Component.Internal
import Hecs.Component.Properties
import Hecs.Component.Relation
import Hecs.Entity.Internal

import Data.Proxy
import Data.Kind
import GHC.TypeLits
import Data.Type.Bool hiding (Not)
import qualified Data.Type.Bool
import GHC.Exts
import GHC.IO (IO(..))
import Data.Bitfield

data FilterContext = HasMainId | DoesNotHaveMainId

type family CombineCtx (l :: FilterContext) (r :: FilterContext) :: FilterContext where
  CombineCtx DoesNotHaveMainId r = r
  CombineCtx l _ = l

type family InvertCtx (ctx :: FilterContext) :: FilterContext where
  InvertCtx HasMainId = DoesNotHaveMainId
  InvertCtx DoesNotHaveMainId = HasMainId

extractMainId :: Filter tyF HasMainId -> ComponentId Any
extractMainId (WithMain i  _) = coerce i
{-# INLINE extractMainId #-}

evaluate :: Filter tyF ctx -> Archetype -> Bool
evaluate (WithMain _ f) = f
evaluate (NotFilter _ f) = f
{-# INLINE evaluate #-}

-- TODO Check if this always fuses cleanly
data Filter (tyF :: k) (ctx :: FilterContext) where
  WithMain :: !(ComponentId Any) -> (Archetype -> Bool) -> Filter tyF HasMainId
  NotFilter :: !(ComponentId Any) -> (Archetype -> Bool) -> Filter tyF DoesNotHaveMainId

data And l r

(.&&.) :: Filter tyL l -> Filter tyR r -> Filter (And tyL tyR) (CombineCtx l r)
WithMain lId f .&&. WithMain _ g = WithMain lId $ \a -> f a && g a
NotFilter _ f .&&. WithMain rId g = WithMain rId $ \a -> f a && g a
WithMain lId f .&&. NotFilter _ g = WithMain lId $ \a -> f a && g a
NotFilter lId f .&&. NotFilter _ g = NotFilter lId $ \a -> f a && g a
{-# INLINE (.&&.) #-}

data Or l r

(.||.) :: Filter tyL l -> Filter tyR r -> Filter (Or tyL tyR) (CombineCtx l r)
WithMain lId f .||. WithMain _ g = WithMain lId $ \a -> f a || g a
NotFilter _ f .||. WithMain rId g = WithMain rId $ \a -> f a || g a
WithMain lId f .||. NotFilter _ g = WithMain lId $ \a -> f a || g a
NotFilter lId f .||. NotFilter _ g = NotFilter lId $ \a -> f a || g a
{-# INLINE (.||.) #-}

data Not x

not :: Filter ty ctx -> Filter (Not ty) (InvertCtx ctx)
not (WithMain m f) = NotFilter m $ Prelude.not . f
not (NotFilter m f) = WithMain m $ Prelude.not . f
{-# INLINE not #-}

type role Tag phantom
data Tag (x :: k)

-- TODO 
-- This has a small inefficiency: The main component id is guaranteed to be there, so no point in rechecking
-- but since we don't know who or what the main id is, we have no choice here
-- TODO Matching Rel Wildcard Wildcard ... (all relations)
componentWithId :: forall c . (BranchRel c, Component c) => ComponentId c -> Filter c HasMainId
componentWithId compId = WithMain (coerce compId) $ \aty ->
  let rel = coerce @_ @(Bitfield Int Relation) compId
      relFirst = coerce (fromIntegral @_ @Int rel.first)
      relSecond = coerce (fromIntegral @_ @Int rel.second)
  in branchRel (Proxy @c)
    (if | relFirst == wildcard && relSecond == wildcard -> lookupWildcardB (Proxy @(ComponentKind c)) aty (const True) False
        | relFirst == wildcard -> lookupWildcardL (Proxy @(ComponentKind c)) aty (coerce $ fromIntegral @_ @Int rel.second) (const True) False
        | relSecond == wildcard -> lookupWildcardR (Proxy @(ComponentKind c)) aty (coerce $ fromIntegral @_ @Int rel.first) (const True) False
        | otherwise -> lookupComponent (Proxy @(ComponentKind c)) aty compId (const True) False
    )
    (lookupComponent (Proxy @(ComponentKind c)) aty compId (const True) False)
{-# INLINE componentWithId #-}

tagWithId :: forall c . BranchRel c => ComponentId c -> Filter c HasMainId
tagWithId compId = WithMain (coerce compId) $ \aty ->
  let rel = coerce @_ @(Bitfield Int Relation) compId
      relFirst = coerce (fromIntegral @_ @Int rel.first)
      relSecond = coerce (fromIntegral @_ @Int rel.second)
      p = Proxy @Hecs.Component.Internal.Tag
  in branchRel (Proxy @c)
    (if | relFirst == wildcard && relSecond == wildcard -> lookupWildcardB p aty (const True) False
        | relFirst == wildcard -> lookupWildcardL p aty compId (const True) False
        | relSecond == wildcard -> lookupWildcardR p aty compId (const True) False
        | otherwise -> lookupComponent p aty compId (const True) False
    )
    (lookupComponent p aty compId (const True) False)
{-# INLINE tagWithId #-}

newtype TypedArchetype ty = TypedArchetype Archetype

-- TODO
getColumnWithId :: forall c ty . (Component c, TypedHas ty c) => TypedArchetype ty -> ComponentId c -> IO (Column (ComponentKind c) c)
getColumnWithId (TypedArchetype aty) compId = lookupComponent (Proxy @(ComponentKind c)) aty compId
  (unsfeGetColumn (Proxy @(ComponentKind c)) aty)
  (error "Hecs.Filter.Internal:getColumn Component that was on the type level wasn't on the value level")
{-# INLINE getColumnWithId #-}

getEntityColumn :: TypedArchetype ty -> IO (Column Flat EntityId)
getEntityColumn (TypedArchetype Archetype{columns = Columns# _ eidsRef _ _ _}) =
  IO $ \s -> case readMutVar# eidsRef s of (# s1, arr #) -> (# s1, ColumnFlat arr #) 

iterateArchetype :: TypedArchetype ty -> (Int -> EntityId -> a -> IO a) -> IO a -> IO a
iterateArchetype (TypedArchetype (Archetype{columns = Columns# szRef eidsRef _ _ _})) f (IO z) = IO $ \s0 ->
  case readIntArray# szRef 0# s0 of
    (# s1, sz #) -> case readMutVar# eidsRef s1 of
      (# s2, eidArr #) -> case z s2 of (# s3, b #) -> go eidArr sz 0# b s3
  where
    go arr sz n b s
      | isTrue# (n >=# sz) = (# s, b #)
      | otherwise = case readIntArray# arr n s of (# s1, eid #) -> case f (I# n) (EntityId $ Bitfield (I# eid)) b of IO g -> case g s1 of (# s2, st #) -> go arr sz (n +# 1#) st s2
{-# INLINE iterateArchetype #-}

type TypedHas :: k -> l -> Constraint
type family TypedHas ty c :: Constraint where
  TypedHas ty c = If (TypedHasBool ty c) (() :: Constraint) (TypeError ('Text "No type level evidence that this archetype has a component typed: " :<>: ShowType c :$$: Text "Type level evidence " :<>: ShowType ty :$$: Text "You may want to use an unsafe access method"))

type TypedHasBool :: k -> l -> Bool
type family TypedHasBool ty c :: Bool where
  TypedHasBool (And l r) c = TypedHasBool l c || TypedHasBool r c
  TypedHasBool (Or l r) c = False -- This is a bit annoying, but if we have an Or, we cannot conclusively say we have a component
  TypedHasBool (Not l) c = Data.Type.Bool.Not (TypedHasBool l c)
  TypedHasBool (Tag c) c = True
  TypedHasBool c c = True
  TypedHasBool a b = False
