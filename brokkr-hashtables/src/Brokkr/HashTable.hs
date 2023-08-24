{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module     : Brokkr.HashTable
-- Copyright  : (c) Jannis Overesch 2023
-- License    : BSD-3 Clause
-- Maintainer : Jannis (overesch.jannis@gmail.com)
--
-- A library containing a flexible and fast mutable hashtable.
--
-- It can store any kind of Haskell key value pair, with the additional
-- option of storing either one or both unboxed.

module Brokkr.HashTable (
  Storage(..)
, HashTable'
, HashTable(..)
, Salt, MaxLoadFactor
, HashFn(..), Hash(..)
) where

import Brokkr.HashTable.Internal

import Brokkr.HashTable.BB qualified as BB
import Brokkr.HashTable.SB qualified as SB
import Brokkr.HashTable.BS qualified as BS
import Brokkr.HashTable.SS qualified as SS
import Brokkr.HashTable.PB qualified as PB
import Brokkr.HashTable.BP qualified as BP
import Brokkr.HashTable.PP qualified as PP
import Brokkr.HashTable.SP qualified as SP
import Brokkr.HashTable.PS qualified as PS

import Foreign.Storable qualified as Storable
import Data.Primitive   qualified as Prim

instance HashTable Boxed Boxed k v where
  new = BB.new
  {-# INLINE new #-}
  size = BB.size
  {-# INLINE size #-}
  reserve = BB.reserve
  {-# INLINE reserve #-}
  lookup = BB.lookup
  {-# INLINE lookup #-}
  lookupWithHash = BB.lookupWithHash
  {-# INLINE lookupWithHash #-}
  insert = BB.insert
  {-# INLINE insert #-}
  insertWithHash = BB.insertWithHash
  {-# INLINE insertWithHash #-}
  delete = BB.delete
  {-# INLINE delete #-}
  deleteWithHash = BB.deleteWithHash
  {-# INLINE deleteWithHash #-}
  foldM = BB.foldM
  {-# INLINE foldM #-}

instance Storable.Storable k => HashTable Storable Boxed k v where
  new = SB.new
  {-# INLINE new #-}
  size = SB.size
  {-# INLINE size #-}
  reserve = SB.reserve
  {-# INLINE reserve #-}
  lookup = SB.lookup
  {-# INLINE lookup #-}
  lookupWithHash = SB.lookupWithHash
  {-# INLINE lookupWithHash #-}
  insert = SB.insert
  {-# INLINE insert #-}
  insertWithHash = SB.insertWithHash
  {-# INLINE insertWithHash #-}
  delete = SB.delete
  {-# INLINE delete #-}
  deleteWithHash = SB.deleteWithHash
  {-# INLINE deleteWithHash #-}
  foldM = SB.foldM
  {-# INLINE foldM #-}

instance Storable.Storable v => HashTable Boxed Storable k v where
  new = BS.new
  {-# INLINE new #-}
  size = BS.size
  {-# INLINE size #-}
  reserve = BS.reserve
  {-# INLINE reserve #-}
  lookup = BS.lookup
  {-# INLINE lookup #-}
  lookupWithHash = BS.lookupWithHash
  {-# INLINE lookupWithHash #-}
  insert = BS.insert
  {-# INLINE insert #-}
  insertWithHash = BS.insertWithHash
  {-# INLINE insertWithHash #-}
  delete = BS.delete
  {-# INLINE delete #-}
  deleteWithHash = BS.deleteWithHash
  {-# INLINE deleteWithHash #-}
  foldM = BS.foldM
  {-# INLINE foldM #-}

instance (Storable.Storable k, Storable.Storable v) => HashTable Storable Storable k v where
  new = SS.new
  {-# INLINE new #-}
  size = SS.size
  {-# INLINE size #-}
  reserve = SS.reserve
  {-# INLINE reserve #-}
  lookup = SS.lookup
  {-# INLINE lookup #-}
  lookupWithHash = SS.lookupWithHash
  {-# INLINE lookupWithHash #-}
  insert = SS.insert
  {-# INLINE insert #-}
  insertWithHash = SS.insertWithHash
  {-# INLINE insertWithHash #-}
  delete = SS.delete
  {-# INLINE delete #-}
  deleteWithHash = SS.deleteWithHash
  {-# INLINE deleteWithHash #-}
  foldM = SS.foldM
  {-# INLINE foldM #-}

instance Prim.Prim k => HashTable Prim Boxed k v where
  new = PB.new
  {-# INLINE new #-}
  size = PB.size
  {-# INLINE size #-}
  reserve = PB.reserve
  {-# INLINE reserve #-}
  lookup = PB.lookup
  {-# INLINE lookup #-}
  lookupWithHash = PB.lookupWithHash
  {-# INLINE lookupWithHash #-}
  insert = PB.insert
  {-# INLINE insert #-}
  insertWithHash = PB.insertWithHash
  {-# INLINE insertWithHash #-}
  delete = PB.delete
  {-# INLINE delete #-}
  deleteWithHash = PB.deleteWithHash
  {-# INLINE deleteWithHash #-}
  foldM = PB.foldM
  {-# INLINE foldM #-}

instance Prim.Prim v => HashTable Boxed Prim k v where
  new = BP.new
  {-# INLINE new #-}
  size = BP.size
  {-# INLINE size #-}
  reserve = BP.reserve
  {-# INLINE reserve #-}
  lookup = BP.lookup
  {-# INLINE lookup #-}
  lookupWithHash = BP.lookupWithHash
  {-# INLINE lookupWithHash #-}
  insert = BP.insert
  {-# INLINE insert #-}
  insertWithHash = BP.insertWithHash
  {-# INLINE insertWithHash #-}
  delete = BP.delete
  {-# INLINE delete #-}
  deleteWithHash = BP.deleteWithHash
  {-# INLINE deleteWithHash #-}
  foldM = BP.foldM
  {-# INLINE foldM #-}

instance (Prim.Prim k, Prim.Prim v) => HashTable Prim Prim k v where
  new = PP.new
  {-# INLINE new #-}
  size = PP.size
  {-# INLINE size #-}
  reserve = PP.reserve
  {-# INLINE reserve #-}
  lookup = PP.lookup
  {-# INLINE lookup #-}
  lookupWithHash = PP.lookupWithHash
  {-# INLINE lookupWithHash #-}
  insert = PP.insert
  {-# INLINE insert #-}
  insertWithHash = PP.insertWithHash
  {-# INLINE insertWithHash #-}
  delete = PP.delete
  {-# INLINE delete #-}
  deleteWithHash = PP.deleteWithHash
  {-# INLINE deleteWithHash #-}
  foldM = PP.foldM
  {-# INLINE foldM #-}

instance (Storable.Storable k, Prim.Prim v) => HashTable Storable Prim k v where
  new = SP.new
  {-# INLINE new #-}
  size = SP.size
  {-# INLINE size #-}
  reserve = SP.reserve
  {-# INLINE reserve #-}
  lookup = SP.lookup
  {-# INLINE lookup #-}
  lookupWithHash = SP.lookupWithHash
  {-# INLINE lookupWithHash #-}
  insert = SP.insert
  {-# INLINE insert #-}
  insertWithHash = SP.insertWithHash
  {-# INLINE insertWithHash #-}
  delete = SP.delete
  {-# INLINE delete #-}
  deleteWithHash = SP.deleteWithHash
  {-# INLINE deleteWithHash #-}
  foldM = SP.foldM
  {-# INLINE foldM #-}

instance (Prim.Prim k, Storable.Storable v) => HashTable Prim Storable k v where
  new = PS.new
  {-# INLINE new #-}
  size = PS.size
  {-# INLINE size #-}
  reserve = PS.reserve
  {-# INLINE reserve #-}
  lookup = PS.lookup
  {-# INLINE lookup #-}
  lookupWithHash = PS.lookupWithHash
  {-# INLINE lookupWithHash #-}
  insert = PS.insert
  {-# INLINE insert #-}
  insertWithHash = PS.insertWithHash
  {-# INLINE insertWithHash #-}
  delete = PS.delete
  {-# INLINE delete #-}
  deleteWithHash = PS.deleteWithHash
  {-# INLINE deleteWithHash #-}
  foldM = PS.foldM
  {-# INLINE foldM #-}
