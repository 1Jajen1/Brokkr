{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Hecs.Component (
  module Hecs.Component.Internal
, module Hecs.Component.Generic
, module Hecs.Component.Relation
, module Hecs.Component.Properties
) where

import Hecs.Component.Internal
  ( Component
  , ComponentId(..)
  , Column
  , ComponentKind
  , AccessColumn(..)
  , ViaBox(..)
  , ViaFlat(..)
  )
import Hecs.Component.Generic
import Hecs.Component.Relation
  ( Rel(..)
  , mkRelation
  , unwrapRelation
  , CaseTag
  , BranchRel
  )
import Hecs.Component.Properties
  ( IsComponent(..), isComponent
  , EntityName(..), entityName
  , WildCard, wildcard
  )
