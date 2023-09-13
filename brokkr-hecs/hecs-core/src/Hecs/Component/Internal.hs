{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Hecs.Component.Internal (
  ComponentId(..)
, EntityId(..)
, Bitfield(..)
--
, ComponentKind(..)
, KnownComponentKind(..)
, Component(..)
, ViaBox(..)
, ViaStorable(..)
) where

import Data.Coerce
import Data.Int
import Data.Kind
import Data.Word
import Data.Void

import Foreign.Storable (Storable)

import GHC.Exts (TYPE)

import Hecs.Entity.Internal

newtype ComponentId (c :: k) = ComponentId { unwrapComponentId :: EntityId }
  deriving newtype (Eq, Show, Storable)

data ComponentKind = Boxed | Flat | Tag

class KnownComponentKind (compKind :: ComponentKind) where
  branchKind :: forall {rep} (r :: TYPE rep) . (compKind ~ Boxed => r) -> (compKind ~ Flat => r) -> (compKind ~ Tag => r) -> r
instance KnownComponentKind Boxed where
  branchKind b _ _ = b
  {-# INLINE branchKind #-}
instance KnownComponentKind Flat where
  branchKind _ f _ = f
  {-# INLINE branchKind #-}
instance KnownComponentKind Tag where
  branchKind _ _ t = t
  {-# INLINE branchKind #-}

class (KnownComponentKind (ComponentKindFor c)) => Component (c :: k) where
  type ComponentKindFor c :: ComponentKind
  type ComponentValueFor c :: Type
  backing
    :: forall {rep} (r :: TYPE rep)
    .  ((ComponentKindFor c ~ Boxed) => r)
    -> ((ComponentKindFor c ~ Flat, Storable (ComponentValueFor c)) => r)
    -> ((ComponentKindFor c ~ Tag, ComponentValueFor c ~ Void) => r)
    -> r
  type ComponentKindFor c = Tag
  type ComponentValueFor c = Void
  default backing :: forall {rep} (r :: TYPE rep)
    .  (ComponentKindFor c ~ Tag, ComponentValueFor c ~ Void)
    => ((ComponentKindFor c ~ Boxed) => r)
    -> ((ComponentKindFor c ~ Flat, Storable (ComponentValueFor c)) => r)
    -> ((ComponentKindFor c ~ Tag, ComponentValueFor c ~ Void) => r)
    -> r
  backing _ _ t = t 

newtype ViaBox a = ViaBox a

instance Component (ViaBox a) where
  type ComponentKindFor (ViaBox a) = Boxed
  type ComponentValueFor (ViaBox a) = a
  backing b _ _ = b
  {-# INLINE backing #-}

newtype ViaStorable a = ViaStorable a

instance Storable a => Component (ViaStorable a) where
  type ComponentKindFor (ViaStorable a) = Flat
  type ComponentValueFor (ViaStorable a) = a
  backing _ f _ = f
  {-# INLINE backing #-}

deriving via (ViaStorable Int  ) instance Component Int
deriving via (ViaStorable Int8 ) instance Component Int8
deriving via (ViaStorable Int16) instance Component Int16
deriving via (ViaStorable Int32) instance Component Int32
deriving via (ViaStorable Int64) instance Component Int64

deriving via (ViaStorable Word  ) instance Component Word
deriving via (ViaStorable Word8 ) instance Component Word8
deriving via (ViaStorable Word16) instance Component Word16
deriving via (ViaStorable Word32) instance Component Word32
deriving via (ViaStorable Word64) instance Component Word64

deriving via (ViaStorable Float ) instance Component Float
deriving via (ViaStorable Double) instance Component Double

deriving via (ViaStorable EntityId) instance Component EntityId
deriving via (ViaStorable (ComponentId a)) instance Component (ComponentId a)
