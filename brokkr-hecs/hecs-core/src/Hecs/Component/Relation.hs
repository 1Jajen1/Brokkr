{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
module Hecs.Component.Relation (
  Rel, mkRel
, Rel'(..)
, mkRelation, unwrapRelation
, Wildcard
) where

import Data.Bitfield
import Data.Coerce
import Data.Kind
import Data.Void

import Foreign.Storable (Storable)

import GHC.TypeLits

import Hecs.Component.Internal
import Hecs.Entity.Internal
import Hecs.World.Has

type Rel (l :: k) (r :: o) = Rel' (ComponentKindFor l) (ComponentKindFor r) l r

data family Rel' (lK :: ComponentKind) (rK :: ComponentKind) (l :: k) (r :: o)
newtype instance Rel' Boxed rK (l :: Type) r = RelBoxedL l
  deriving newtype Eq
newtype instance Rel' Flat rK (l :: Type) r = RelFlatL l
  deriving newtype (Eq, Storable)
newtype instance Rel' Tag Boxed l (r :: Type) = RelBoxedR r
  deriving newtype Eq
newtype instance Rel' Tag Flat l (r :: Type) = RelFlatR r
  deriving newtype (Eq, Storable)
data instance Rel' Tag Tag l r

class MkRel l r (lk :: ComponentKind) (rk :: ComponentKind) where
  type ArgRel l r lk rk :: Type
  mkRel :: ArgRel l r lk rk -> Rel' lk rk l r

instance ComponentKindFor l ~ Boxed => MkRel (l :: Type) r Boxed rk where
  type ArgRel l r Boxed rk = l
  mkRel = RelBoxedL
instance ComponentKindFor l ~ Flat => MkRel (l :: Type) r Flat rk where
  type ArgRel l r Flat rk = l
  mkRel = RelFlatL
instance (ComponentKindFor l ~ Tag, ComponentKindFor r ~ Boxed) => MkRel l (r :: Type) Tag Boxed where
  type ArgRel l r Tag Boxed = r
  mkRel = RelBoxedR
instance (ComponentKindFor l ~ Tag, ComponentKindFor r ~ Flat) => MkRel l (r :: Type) Tag Flat where
  type ArgRel l r Tag Flat = r
  mkRel = RelFlatR
instance ( TypeError
  ( Text "Cannot create a value Rel " :<>: ShowType l :<>: Text " " :<>: ShowType r
    :$$: Text "A relation of two tag components is also a tag component"
    :$$: Text "Tag components cannot have values"
  )) => MkRel l r Tag Tag where
  mkRel = error "Unreachable"

instance (Component l, Component r, ComponentKindFor l ~ Boxed, ComponentKindFor r ~ rk) => Component (Rel' Boxed rk l r) where
  type ComponentKindFor (Rel' Boxed rk l r) = Boxed
  type ComponentValueFor (Rel' Boxed rk l r) = ComponentValueFor l
  backing b _ _ = b

instance (Component l, Component r, Storable (ComponentValueFor l), ComponentKindFor l ~ Flat, ComponentKindFor r ~ rk) => Component (Rel' Flat rk l r) where
  type ComponentKindFor (Rel' Flat rk l r) = Flat
  type ComponentValueFor (Rel' Flat rk l r) = ComponentValueFor l
  backing _ f _ = f

instance (Component l, Component r, ComponentKindFor l ~ Tag, ComponentKindFor r ~ Boxed) => Component (Rel' Tag Boxed l r) where
  type ComponentKindFor (Rel' Tag Boxed l r) = Boxed
  type ComponentValueFor (Rel' Tag Boxed l r) = ComponentValueFor r
  backing b _ _ = b

instance (Component l, Component r, Storable (ComponentValueFor r), ComponentKindFor l ~ Tag, ComponentKindFor r ~ Flat) => Component (Rel' Tag Flat l r) where
  type ComponentKindFor (Rel' Tag Flat l r) = Flat
  type ComponentValueFor (Rel' Tag Flat l r) = ComponentValueFor r
  backing _ f _ = f

instance (Component l, Component r, ComponentKindFor l ~ Tag, ComponentKindFor r ~ Tag) => Component (Rel' Tag Tag l r) where
  type ComponentKindFor (Rel' Tag Tag l r) = Tag
  type ComponentValueFor (Rel' Tag Tag l r) = Void
  backing _ _ t = t

-- TODO Doc: Don't make relations out of relations, stick to one level please

-- | Create a component id for a relation
mkRelation :: ComponentId l -> ComponentId r -> ComponentId (Rel l r)
{-# INLINE mkRelation #-}
mkRelation (ComponentId (EntityId (unwrap -> !l))) (ComponentId (EntityId (unwrap -> !r))) = ComponentId (EntityId $! coerce combined)
  where
    combined :: Bitfield Int Relation
    {-# INLINE combined #-}
    combined = pack $ Relation (fromIntegral l) (fromIntegral r) . pack $ EntityTag True

-- | Retrieve the component ids used to create a relation
unwrapRelation :: ComponentId (Rel l r) -> (ComponentId l, ComponentId r)
{-# INLINE unwrapRelation #-}
unwrapRelation (ComponentId (EntityId (coerce @_ @(Bitfield Int Relation) -> !b))) = (mk . fromIntegral $ get @"first" b, mk . fromIntegral $ get @"second" b)
  where mk = ComponentId . EntityId . Bitfield

data Wildcard
  deriving anyclass Component

-- See Preset.hs for a list of ids
instance Has w Wildcard where
  getComponentId _ = ComponentId . EntityId $ Bitfield 5
  {-# INLINE getComponentId #-}

instance (Has w l, Has w r, lk ~ ComponentKindFor l, rk ~ ComponentKindFor r) => Has w (Rel' lk rk l r) where
  getComponentId p = mkRelation (getComponentId p) (getComponentId p)
  {-# INLINE getComponentId #-}
