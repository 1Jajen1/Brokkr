{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Hecs.Component.Internal (
  ComponentId(..)
, Component(..)
, ComponentType(..)
, Column(..)
, ViaBox(..)
, ViaFlat(..)
, AccessColumn(..)
, KnownComponentType(..)
) where

import Hecs.Entity.Internal
import Foreign.Storable
import Data.Proxy
import GHC.Exts
import Data.Kind
import Data.Int
import Data.Word
import GHC.IO (IO(IO))
import Control.Monad.Base

-- | Component id
--
-- Components are entities. Thus a component id is a newtype around 'EntityId'.
-- This enables dynamic components. The newtype is also fully exported to allow
-- the manual construction of component ids. Non-tag components will still need
-- a 'Component' instance!
newtype ComponentId (c :: k) = ComponentId EntityId
  deriving newtype (Eq, Show, Storable)

-- | Hecs supports 3 component types
--
-- * Boxed components are stored as haskell heap objects in either 'Array#' or 'SmallArray#'
-- * Flat components use a storable instance to store data directly in 'ByteArray#''s
-- * Tags are not stored per entity. Instead the archetype's keep track of their own components
--   and as such also know which tags every entity has 
data ComponentType = Boxed | Flat | Tag

-- Can be used to branch on the component type without knowing the actual type.
-- This is used to specialize functions that need to know the storage kind but not the type
-- In turn this removes a lot of excessive inlining and the only orphan specialisation done
--  by TH is for setting a component (to specialise the Storable constraint)

-- | Branch on the term level on a type level component type
--
-- Very useful for some of the type level hackery involved in making all these
-- component types work on one internal interface!
class KnownComponentType (ty :: ComponentType) where
  branchCompType :: Proxy ty -> (ty ~ Boxed => r) -> (ty ~ Flat => r) -> (ty ~ Tag => r) -> r

instance KnownComponentType Boxed where
  branchCompType _ a _ _ = a
  {-# INLINE branchCompType #-}

instance KnownComponentType Flat where
  branchCompType _ _ b _ = b
  {-# INLINE branchCompType #-}

instance KnownComponentType Tag where
  branchCompType _ _ _ c = c
  {-# INLINE branchCompType #-}

-- | Constraints and class that all non-tag components have to implement in order to
-- get/set values.
--
-- Requires static knowledge about the component type.
--
-- Allows term level branching on the type level component kind, but compared to
-- 'KnownComponentType' requires either boxed or flat components.
class (Coercible (Value c) c, KnownComponentType (ComponentKind c)) => Component c where
  type ComponentKind c :: ComponentType
  -- This is some hackery to allow newtypes to more easily derive this class
  -- In short most if not all users will be fine with 'ViaBox', 'ViaFlat' and 'GenericFlat'
  type Value c :: Type
  backing :: Proxy c -> (ComponentKind c ~ Boxed => r) -> ((ComponentKind c ~ Flat, Storable (Value c)) => r) -> r

-- | Data family for working with either boxed or flat data arrays for one component
--
-- Often retrieved from typed archetypes during iteration.
--
-- 'AccessColumn' contains methods for working with this data-structure
data family Column (ty :: ComponentType) c
data instance Column Boxed c = ColumnBoxed (MutableArray# RealWorld c)
data instance Column Flat  c = ColumnFlat  (MutableByteArray# RealWorld)

-- | Typeclass for working with 'Column's
--
-- Allows array like access to all component values
--
-- Warning: Access does no bounds checking! Usually indices are passed
-- from 'iterateArchetype' which is always safe.
class AccessColumn (ty :: ComponentType) c where
  readColumn :: MonadBase IO m => Column ty c -> Int -> m c
  writeColumn :: MonadBase IO m => Column ty c -> Int -> c -> m ()

instance AccessColumn Boxed c where
  readColumn (ColumnBoxed arr) (I# n) = liftBase $ IO (readArray# arr n)
  {-# INLINE readColumn #-}
  writeColumn (ColumnBoxed arr) (I# n) el = liftBase $ IO $ \s -> case writeArray# arr n el s of s1 -> (# s1, () #)
  {-# INLINE writeColumn #-}

instance Storable c => AccessColumn Flat c where
  readColumn (ColumnFlat arr) n = liftBase $ peekElemOff (Ptr (mutableByteArrayContents# arr)) n
  {-# INLINE readColumn #-}
  writeColumn (ColumnFlat arr) n el = liftBase $ pokeElemOff (Ptr (mutableByteArrayContents# arr)) n el
  {-# INLINE writeColumn #-}

-- | Newtype for use with deriving via to derive 'Component'
--
-- Instructs ecs to store the component as boxed haskell values
--
-- See 'ViaFlat' or 'GenericFlat' for flat storage
newtype ViaBox a = ViaBox a

instance Component (ViaBox a) where
  type ComponentKind (ViaBox a) = Boxed
  type Value (ViaBox a) = a
  backing _ b _ = b
  {-# INLINE backing #-}

-- | Newtype for use with deriving via to derive 'Component'
--
-- Instructs ecs to store the component as raw bytes
--
-- See 'ViaBox' to instead store as boxed values
-- See 'GenericFlat' if you want to also generically derive 'Storable'
newtype ViaFlat a = ViaFlat a
  deriving newtype Storable

instance Storable a => Component (ViaFlat a) where
  type ComponentKind (ViaFlat a) = Flat
  type Value (ViaFlat a) = a
  backing _ _ f = f
  {-# INLINE backing #-}

deriving via (ViaFlat Int  ) instance Component Int
deriving via (ViaFlat Int8 ) instance Component Int8
deriving via (ViaFlat Int16) instance Component Int16
deriving via (ViaFlat Int32) instance Component Int32
deriving via (ViaFlat Int64) instance Component Int64

deriving via (ViaFlat Word  ) instance Component Word
deriving via (ViaFlat Word8 ) instance Component Word8
deriving via (ViaFlat Word16) instance Component Word16
deriving via (ViaFlat Word32) instance Component Word32
deriving via (ViaFlat Word64) instance Component Word64

deriving via (ViaFlat Float ) instance Component Float
deriving via (ViaFlat Double) instance Component Double
deriving via (ViaFlat EntityId) instance Component EntityId
deriving via (ViaFlat (ComponentId a)) instance Component (ComponentId a)
