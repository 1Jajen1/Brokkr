{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module Hecs.Component.Preset (
  EntityName(..)
) where

import Data.Bitfield
import Data.String (IsString)
import Data.Text (Text)

import Hecs.Entity.Internal
import Hecs.Component.Internal
import Hecs.World.Has

newtype EntityName = EntityName Text
  deriving newtype (Eq, Show, IsString)
  deriving Component via ViaBox EntityName

{-

List of preset entity ids: (not defined here because of cycles and hs-boot leads to weird inline behavior)
- EntityName : 1
- SomeQuery  : 2
- System     : 3
- DependsOn  : 4
- Wildcard   : 5
-}

instance Has w EntityName where
  getComponentId _ = ComponentId . EntityId $ Bitfield 1
  {-# INLINE getComponentId #-}



