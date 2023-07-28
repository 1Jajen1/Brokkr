module Brokkr.PackedVector (
  DynamicNat(..)
, PackedVector
, Mutable
-- creation
, fromList
, unsafeFromForeignPtr
, unsafeBacking
-- length
, null, length, bitSize
-- indexing
, unsafeIndex, (!), (!?)
-- copy
, unsafeThaw, thaw
, unsafeFreeze, freeze
, force
-- folds
, toList
-- pack typeclass for constraints
, Pack
, PVector
) where

import Prelude hiding (null, length)

import Brokkr.PackedVector.Pack

import Brokkr.PackedVector.Internal
