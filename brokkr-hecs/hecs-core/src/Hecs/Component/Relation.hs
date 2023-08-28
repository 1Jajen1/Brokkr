{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Hecs.Component.Relation (
  Rel(..)
, mkRelation
, unwrapRelation
, CaseTag
, BranchRel(..)
) where

import Hecs.Component.Internal
import Hecs.Entity.Internal
import {-# SOURCE #-} Hecs.Filter.Internal (Tag)
import Data.Proxy
import Foreign.Storable
import Data.Void
import GHC.TypeLits
import Data.Coerce
import Data.Kind
import Data.Bitfield
import Data.String (IsString(..))
import Data.Int
import Data.Word

-- Type voodoo ahead, continue at your own risk

{- Notes:
A relation is:
  - A tag if both types are tags. In this case it is represented as Void, but a value should never be needed
  - The left component type, if the left type is a component
  - The right component type, if the left type is a tag and the right type is a component

This means a relation is also a component if either the left or right type is a component.

The type instance CaseTag is created by makeWorld with template haskell. Since each type has to be registered there (for static component ids) we know this always reduces.
The logic is as follows: If a type ty has an instance Component ty in scope, it always becomes a component, otherwise it is a tag. Orphan instances break this and can thus not be
used in Rel, so avoid them at all cost.

How the actual type machinery works:
- CaseTag determines for each type if a type is a tag
- Component and ComponentKind determine for each type if the component is stored flat or boxed. In case of flat, backing will introduce a Storable constraint
- Switch provides a way to bring the type level check of what part is a tag to the value level. It can introduce constraints based on which type is a tag.
  It does not provide an alternative for tags because those don't need instances (they are empty types)
-}

-- | A relation of two components
--
-- A relation is either a tag or a full component:
-- * If both parts are tags, the relation is a tag
-- * If the left is not a tag, the relation stores the left component
-- * Otherwise the relation stores the right component
--
-- This requires a little bit of type machinery. For static components TH will set this up just fine.
-- For dynamic components, a instance of 'CaseTag' is required, unless the dynamic component is just
-- a tag, in which case specify the Relation as 'Rel (Tag x) r'.
newtype Rel l r = Rel (CaseTag l (CaseTag r Void r) l)

-- | Type level branching on whether or not 'k' is a tag component
type CaseTag :: k -> x -> x -> x
type family CaseTag a b c

type instance CaseTag (Rel l r) a b = CaseTag l (CaseTag r a b) b
type instance CaseTag (Tag x) a _ = a

-- Instances for things we provide Component instances for
type instance CaseTag EntityId _ b = b
type instance CaseTag (ComponentId _) _ b = b
type instance CaseTag Int _ b = b
type instance CaseTag Int8 _ b = b
type instance CaseTag Int16 _ b = b
type instance CaseTag Int32 _ b = b
type instance CaseTag Int64 _ b = b
type instance CaseTag Word _ b = b
type instance CaseTag Word8 _ b = b
type instance CaseTag Word16 _ b = b
type instance CaseTag Word32 _ b = b
type instance CaseTag Word64 _ b = b
type instance CaseTag Float _ b = b
type instance CaseTag Double _ b = b

type ClassContext ctx l r =
  Switch (CaseTag l True False) (CaseTag r True False)
    (ctx l, CaseTag l (CaseTag r Void r) l ~ l)
    (ctx r, CaseTag l (CaseTag r Void r) l ~ r)

classCtx :: forall ctx l r a . ClassContext ctx l r => ((ctx l, CaseTag l (CaseTag r Void r) l ~ l) => a) -> ((ctx r, CaseTag l (CaseTag r Void r) l ~ r) => a) -> a
classCtx a b = switch @(CaseTag l True False) @(CaseTag r True False)
    @(ctx l, CaseTag l (CaseTag r Void r) l ~ l)
    @(ctx r, CaseTag l (CaseTag r Void r) l ~ r) a b
{-# INLINE classCtx #-}

instance ClassContext Eq l r => Eq (Rel l r) where
  Rel x == Rel y = classCtx @Eq @l @r
    (x == y) (x == y)
  {-# INLINE (==) #-}

instance ClassContext Show l r => Show (Rel l r) where
  show (Rel x) = classCtx @Show @l @r
    (show x) (show x)
  {-# INLINE show #-}

instance ClassContext Enum l r => Enum (Rel l r) where
  toEnum i = Rel $ classCtx @Enum @l @r
    (toEnum i) (toEnum i)
  {-# INLINE toEnum #-}
  fromEnum (Rel i) = classCtx @Enum @l @r
    (fromEnum i) (fromEnum i)
  {-# INLINE fromEnum #-}

instance ClassContext Num l r => Num (Rel l r) where
  Rel l + Rel r = Rel $ classCtx @Num @l @r (l + r) (l + r)
  Rel l * Rel r = Rel $ classCtx @Num @l @r (l * r) (l * r)
  abs (Rel x) = Rel $ classCtx @Num @l @r (abs x) (abs x)
  signum (Rel x) = Rel $ classCtx @Num @l @r (signum x) (signum x)
  Rel l - Rel r = Rel $ classCtx @Num @l @r (l - r) (l - r)
  fromInteger x = Rel $ classCtx @Num @l @r (fromInteger x) (fromInteger x)

instance (ClassContext Eq l r, ClassContext Ord l r) => Ord (Rel l r) where
  compare (Rel l) (Rel r) = classCtx @Ord @l @r (compare l r) (compare l r)

instance (ClassContext Eq l r, ClassContext Ord l r, ClassContext Num l r, ClassContext Real l r) => Real (Rel l r) where
  toRational (Rel x) = classCtx @Real @l @r (toRational x) (toRational x)

instance (ClassContext Eq l r, ClassContext Ord l r, ClassContext Num l r, ClassContext Real l r, ClassContext Enum l r, ClassContext Integral l r) => Integral (Rel l r) where
  toInteger (Rel x) = classCtx @Integral @l @r (toInteger x) (toInteger x)
  quotRem (Rel a) (Rel b) = case classCtx @Integral @l @r (quotRem a b) (quotRem a b) of
    (q,r) -> (Rel q, Rel r)

instance ClassContext IsString l r => IsString (Rel l r) where
  fromString str = Rel $ classCtx @IsString @l @r (fromString str) (fromString str)

type StorableCtx l r = ClassContext Storable l r

instance StorableCtx l r => Storable (Rel l r) where
    sizeOf _ = classCtx @Storable @l @r
      (sizeOf $ undefined @_ @l)
      (sizeOf $ undefined @_ @r)
    {-# INLINE sizeOf #-}
    alignment _ = classCtx @Storable @l @r
      (alignment $ undefined @_ @l)
      (alignment $ undefined @_ @r)
    {-# INLINE alignment #-}
    peek ptr = classCtx @Storable @l @r
      (Rel <$> peek @l (coerce ptr))
      (Rel <$> peek @r (coerce ptr))
    {-# INLINE peek #-}
    peekElemOff ptr = classCtx @Storable @l @r
      (fmap Rel . peekElemOff @l (coerce ptr))
      (fmap Rel . peekElemOff @r (coerce ptr))
    {-# INLINE peekElemOff #-}
    peekByteOff ptr = classCtx @Storable @l @r
      (fmap Rel . peekByteOff @l (coerce ptr))
      (fmap Rel . peekByteOff @r (coerce ptr))
    {-# INLINE peekByteOff #-}
    poke ptr (Rel x) = classCtx @Storable @l @r
      (poke @l (coerce ptr) x)
      (poke @r (coerce ptr) x)
    {-# INLINE poke #-}
    pokeElemOff ptr n (Rel x) = classCtx @Storable @l @r
      (pokeElemOff @l (coerce ptr) n x)
      (pokeElemOff @r (coerce ptr) n x)
    {-# INLINE pokeElemOff #-}
    pokeByteOff ptr n (Rel x) = classCtx @Storable @l @r
      (pokeByteOff @l (coerce ptr) n x)
      (pokeByteOff @r (coerce ptr) n x)
    {-# INLINE pokeByteOff #-}

class Switch (b1 :: Bool) (b2 :: Bool) ctx1 ctx2 where
  switch :: (ctx1 => a) -> (ctx2 => a) -> a

instance ctx1 => Switch False True ctx1 ctx2 where
  switch a _ = a
  {-# INLINE switch #-}

instance ctx1 => Switch False False ctx1 ctx2 where
  switch a _ = a
  {-# INLINE switch #-}

instance ctx2 => Switch True False ctx1 ctx2 where
  switch _ a = a
  {-# INLINE switch #-}

instance TypeError ('Text "Cannot use a tag relation as a component") => Switch True True ctx1 ctx2 where

type family CaseStorable (b :: ComponentType) (ctx :: Constraint) :: Constraint where
  CaseStorable Boxed _ = ()
  CaseStorable Flat ctx = ctx

instance 
     ( Switch (CaseTag l True False) (CaseTag r True False)
        (Component l, CaseTag l (CaseTag r (ComponentKind r) (ComponentKind r)) (ComponentKind l) ~ ComponentKind l, CaseStorable (ComponentKind l) (StorableCtx l r))
        (Component r, CaseTag l (CaseTag r (ComponentKind r) (ComponentKind r)) (ComponentKind l) ~ ComponentKind r, CaseStorable (ComponentKind r) (StorableCtx l r))
     , KnownComponentType (ComponentKind (Rel l r))
     )
  => Component (Rel l r)
  where
    type ComponentKind (Rel l r) = CaseTag l (CaseTag r (ComponentKind r) (ComponentKind r)) (ComponentKind l)
    type (Value (Rel l r)) = Rel l r
    backing _ b f = switch @(CaseTag l True False) @(CaseTag r True False)
      @(Component l, CaseTag l (CaseTag r (ComponentKind r) (ComponentKind r)) (ComponentKind l) ~ ComponentKind l, CaseStorable (ComponentKind l) (StorableCtx l r))
      @(Component r, CaseTag l (CaseTag r (ComponentKind r) (ComponentKind r)) (ComponentKind l) ~ ComponentKind r, CaseStorable (ComponentKind r) (StorableCtx l r))
      (backing (Proxy @l) b f)
      (backing (Proxy @r) b f)
    {-# INLINE backing #-}

-- | Create a component id for a relation
mkRelation :: ComponentId l -> ComponentId r -> ComponentId (Rel l r)
mkRelation (ComponentId (EntityId (unwrap -> l))) (ComponentId (EntityId (unwrap -> r))) = ComponentId (EntityId $ coerce combined)
  where
    combined :: Bitfield Int Relation
    combined = pack $ Relation (fromIntegral l) (fromIntegral r) . pack $ EntityTag True
{-# INLINE mkRelation #-}

-- | Retrieve the component ids used to create a relation
unwrapRelation :: ComponentId (Rel l r) -> (ComponentId l, ComponentId r)
unwrapRelation (ComponentId (EntityId (coerce @_ @(Bitfield Int Relation) -> b))) = (mk . fromIntegral $ get @"first" b, mk . fromIntegral $ get @"second" b)
  where mk = ComponentId . EntityId . Bitfield
{-# INLINE unwrapRelation #-}

-- | Branch on the term level if 'c' is a relation
--
-- This is currently quite the footgun when writing custom combinators using things
-- that have a 'BranchRel' constraint. Make sure to propagate it regardless of what
-- ghc tells you!
class BranchRel (c :: k) where
  branchRel :: Proxy c
    -> r -- c is Rel l r
    -> r -- c is none of the above
    -> r

instance {-# OVERLAPS #-} BranchRel (Rel l r) where
  branchRel _ a _ = a
  {-# INLINE branchRel #-}


instance {-# OVERLAPPABLE #-} BranchRel c where
  branchRel _ _ a = a
  {-# INLINE branchRel #-}
