{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Hecs.Component.Properties (
  IsComponent(..)
, EntityName(..)
-- , IsA
, WildCard
, internalTypes
, isComponent, entityName, {-isA,-} wildcard
) where

import Data.Bitfield
import Data.Text
import Data.Typeable

import Hecs.Component.Internal
import Language.Haskell.TH
import Hecs.World.Has
import Hecs.Entity.Internal
import Hecs.Component.Relation
import Data.String (IsString)

-- This module is mostly experimental. When I start debugging tools this will be very useful!

-- | Identify components as such. This is only useful for dynamic contexts (script boundaries, debugging, ...)
--
-- Automatically set for all static components during setup when using the TH setup
data IsComponent where
  IsComponent :: forall c . (Typeable c, Component c) => IsComponent
  deriving Component via ViaBox IsComponent
type instance CaseTag IsComponent _ b = b

instance Has w IsComponent where
  getComponentId _ = isComponent
  {-# INLINE getComponentId #-}

-- | Global component id for 'IsComponent'
isComponent :: ComponentId IsComponent
isComponent = ComponentId . EntityId $ Bitfield 1
{-# INLINE isComponent #-}

-- | Entities can be named, not mandatory, but useful for debugging
newtype EntityName = EntityName Text
  deriving newtype (Eq, Show, IsString)
  deriving Component via ViaBox EntityName
type instance CaseTag EntityName _ b = b

instance Has w EntityName where
  getComponentId _ = entityName
  {-# INLINE getComponentId #-}

-- | Global component id for 'EntityName'
entityName :: ComponentId EntityName
entityName = ComponentId . EntityId $ Bitfield 2
{-# INLINE entityName #-}

-- Specify an "is" relation between two entities.
-- If a has Rel IsA b as a tag, then a includes all components
-- of b. TODO Add the components on setting the rel or defer
-- until the components are actuall written to?
data IsA
type instance CaseTag IsA a _ = a

instance Has w IsA where
  getComponentId _ = isA
  {-# INLINE getComponentId #-}

isA :: ComponentId IsA
isA = ComponentId . EntityId $ Bitfield 3
{-# INLINE isA #-}

-- | Relation wildcard
--
-- Filters can use this wildcard to match all relations containing the other component
data WildCard
type instance CaseTag WildCard a _ = a

instance Has w WildCard where
  getComponentId _ = wildcard
  {-# INLINE getComponentId #-}

-- | Global component id for 'WildCard'
wildcard :: ComponentId WildCard
wildcard = ComponentId . EntityId $ Bitfield 4
{-# INLINE wildcard #-}

-- Match with the list passed to genInternal

-- | List of 'Name' for TH to generate correct setup code
internalTypes :: [Name]
internalTypes = [''IsComponent, ''EntityName, ''IsA, ''WildCard, ''EntityId]
