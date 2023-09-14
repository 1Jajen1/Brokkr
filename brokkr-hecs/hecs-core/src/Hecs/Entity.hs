{-# LANGUAGE DuplicateRecordFields #-}
module Hecs.Entity (
  EntityId
, EntitySparseSet
, new
, allocateEntityId
, deAllocateEntityId
, isAlive
, insert, lookup
, Entity(eid,generation,tag)
, EntityTag(isRelation)
, Relation(first, second, tag)
) where

import Prelude hiding (lookup)

import Hecs.Entity.Internal
