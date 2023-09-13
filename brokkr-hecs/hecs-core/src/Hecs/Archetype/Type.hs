{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Hecs.Archetype.Type (
  ArchetypeType(..)
, addComponentType
, removeComponentType
, findComponent
, findRelationMatches
, iterateComponents
, linearSearch
) where

import Control.Monad.ST.Strict (runST)

import Data.Bits
import Data.Bitfield
import Data.Coerce
import Data.Hashable
import Data.Primitive
import Data.Proxy

import GHC.Exts

import Hecs.Component.Internal
import Hecs.Component.Relation
import Hecs.Entity.Internal
import Hecs.World.Has

data ArchetypeType = ArchetypeType
  {-# UNPACK #-} !Int             -- hash
  {-# UNPACK #-} !(PrimArray Int) -- boxed components
  {-# UNPACK #-} !(PrimArray Int) -- flat  components
  {-# UNPACK #-} !(PrimArray Int) -- tag   components
  {-# UNPACK #-} !Relations       -- Efficient wildcard/any lookups
  deriving stock (Eq, Show)

{- NOTE: Relation lookups

Relations are stored in their respective component type arrays, but also in a separate
structure. On wildcard lookups this structure is searched instead.

TODO Maintain sorted-ness in relations?
-}

data Relations = Relations
  {-# UNPACK #-} !(PrimArray Int) -- stores all relations
  {-# UNPACK #-} !(PrimArray Int) -- index into the boxed/flat/tag arrays
  deriving stock (Eq, Show)

instance Semigroup Relations where
  Relations lR lI <> Relations rR rI = Relations (lR <> rR) (lI <> rI)
instance Monoid Relations where
  mempty = Relations mempty mempty

-- TODO Verify that the compiled functions don't generate weird code. Had bad experience with
-- this code before in the nbt parser

addComponentType :: forall c . Component c => ArchetypeType -> ComponentId c -> ArchetypeType
{-# INLINE addComponentType #-}
addComponentType (ArchetypeType _ b f t rels) cid0 = backing @_ @c
  (let (b', i) = insertSorted b (coerce cid0) in ArchetypeType (hashComponents b' f  t ) b' f  t  $ rels' i)
  (let (f', i) = insertSorted f (coerce cid0) in ArchetypeType (hashComponents b  f' t ) b  f' t  $ rels' i)
  (let (t', i) = insertSorted t (coerce cid0) in ArchetypeType (hashComponents b  f  t') b  f  t' $ rels' i)
  where
    rels' ind = if (coerce @_ @(Bitfield Int Entity) cid0).tag.isRelation then addRelation rels (coerce cid0) ind else rels
    -- TODO ghc does not remove the tuple box
    insertSorted arr cid = runST $ do
      let sz = sizeofPrimArray arr
      marr <- newPrimArray (sz + 1)
      let ind = case linearSearch arr cid of
                  (# n | #) -> I# n
                  (# | n #) -> I# n
      copyPrimArray marr 0 arr 0 ind
      writePrimArray marr ind cid
      copyPrimArray marr (ind + 1) arr ind (sz - ind)
      (,ind) <$> unsafeFreezePrimArray marr

addRelation :: Relations -> ComponentId (Rel a b) -> Int -> Relations
{-# INLINE addRelation #-}
addRelation (Relations rels inds) cid ind = runST $ do
    mRels <- newPrimArray (sz + 1)
    mInds <- newPrimArray (sz + 1)
    copyPrimArray mRels 0 rels 0 sz
    copyPrimArray mInds 0 inds 0 sz
    writePrimArray mRels sz (coerce cid)
    writePrimArray mInds sz ind
    Relations <$> unsafeFreezePrimArray mRels <*> unsafeFreezePrimArray mInds
  where
    sz = sizeofPrimArray rels

removeRelation :: Relations -> ComponentId (Rel a b) -> Relations
{-# INLINE removeRelation #-}
removeRelation (Relations rels inds) cid = runST $ do
    mRels <- newPrimArray (sz - 1)
    mInds <- newPrimArray (sz - 1)
    copyPrimArray mRels 0 rels 0 ind
    copyPrimArray mInds 0 inds 0 ind
    copyPrimArray mRels ind rels (ind + 1) (sz - ind - 1)
    copyPrimArray mInds ind inds (ind + 1) (sz - ind - 1)
    Relations <$> unsafeFreezePrimArray mRels <*> unsafeFreezePrimArray mInds
  where
    sz = sizeofPrimArray rels
    goFindRemove !n
      | n >= sz = error "Not in"
      | coerce ind == indexPrimArray rels n = n
      | otherwise = goFindRemove (n + 1)
    ind = goFindRemove 0

removeComponentType :: forall c . Component c => ArchetypeType -> ComponentId c -> ArchetypeType
{-# INLINE removeComponentType #-}
removeComponentType (ArchetypeType _ b f t rels) cid0 = backing @_ @c
  (let b' = removeSorted b (coerce cid0) in ArchetypeType (hashComponents b' f  t ) b' f  t  rels')
  (let f' = removeSorted f (coerce cid0) in ArchetypeType (hashComponents b  f' t ) b  f' t  rels')
  (let t' = removeSorted t (coerce cid0) in ArchetypeType (hashComponents b  f  t') b  f  t' rels')
  where
    rels' = if (coerce @_ @(Bitfield Int Entity) cid0).tag.isRelation then removeRelation rels (coerce cid0) else rels
    removeSorted arr cid = runST $ do
      let sz = sizeofPrimArray arr
      marr <- newPrimArray (sz - 1)
      let ind = case linearSearch arr cid of
                  (# n | #) -> I# n
                  (# | _ #) -> error "removeComponentType:Invariant:Type did not have component to remove!"
      copyPrimArray marr 0 arr 0 ind
      copyPrimArray marr ind arr (ind + 1) (sz - ind - 1)
      unsafeFreezePrimArray marr

findComponent :: forall c r . Component c => ArchetypeType -> ComponentId c -> (Int -> r) -> r -> r
{-# INLINE findComponent #-}
findComponent (ArchetypeType _ b f t _) cid onSucc onFail =
  case backing @_ @c @(# Int# | Int# #)
    (linearSearch b (coerce cid))
    (linearSearch f (coerce cid))
    (linearSearch t (coerce cid))
    of
      (# n | #) -> onSucc (I# n)
      (# | _ #) -> onFail

-- Taking the wildcard as an argument is annoying
-- TODO Resolve the Component/Preset issues better...
findRelationMatches :: forall a b r . ArchetypeType -> ComponentId (Rel a b) -> (Int -> Int -> r -> r) -> r -> r
{-# INLINE findRelationMatches #-}
findRelationMatches (ArchetypeType _ _ _ _ (Relations rels inds)) !cid f z
  | coerce @_ @Int relFst == coerce wildcard && coerce @_ @Int relSecond == coerce wildcard = goAll 0
  | coerce @_ @Int relFst == coerce wildcard    = goSearch (\c -> c .&. maskHigh) (coerce relSecond `unsafeShiftL` 32) 0
  | coerce @_ @Int relSecond == coerce wildcard = goSearch (\c -> c .&. maskLow)  (coerce relFst) 0
  | otherwise = z
  where
    wildcard :: ComponentId Wildcard = getComponentId (Proxy @Int)
    sz = sizeofPrimArray rels
    (!relFst, !relSecond) = unwrapRelation cid
    goAll !n
      | n >= sz = z
      | otherwise = f (indexPrimArray rels n) (indexPrimArray inds n) (goAll (n + 1))
    maskLow :: Int
    maskLow = (1 `unsafeShiftL` 32) - 1
    maskHigh = ((1 `unsafeShiftL` 24) - 1) `unsafeShiftL` 32
    goSearch !mask !cid !n
      | n >= sz = inline z
      | el <- indexPrimArray rels n
      , mask el == cid = f el (indexPrimArray inds n) (goSearch mask cid (n + 1))
      | otherwise = goSearch mask cid (n + 1)
    {-# INLINE goSearch #-} -- To inline the mask

-- Move to utils
linearSearch :: PrimArray Int -> Int -> (# Int# | Int# #)
linearSearch arr !x = goLinearSearch 0
  where
    sz@(I# sz#) = sizeofPrimArray arr
    goLinearSearch n@(I# n#)
      | n >= sz = (# | sz# #)
      | otherwise = case compare (indexPrimArray arr n) x of
        EQ -> (# n# | #)
        LT -> goLinearSearch (n + 1)
        GT -> (# | n# #)

hashComponents :: PrimArray Int -> PrimArray Int -> PrimArray Int -> Int
{-# INLINE hashComponents #-}
hashComponents (PrimArray b) (PrimArray f) (PrimArray t) =
  hashWithSalt (hashWithSalt (hash (ByteArray b)) (ByteArray f)) (ByteArray t)

iterateComponents :: ArchetypeType -> (forall c . ComponentId c -> Int -> IO ()) -> IO ()
iterateComponents (ArchetypeType _ b f t _) func = do
  goArr b (sizeofPrimArray b) 0
  goArr f (sizeofPrimArray f) 0
  goArr t (sizeofPrimArray t) 0
  where
    goArr !arr !sz !n
      | n >= sz = pure ()
      | otherwise =
        func (coerce $ indexPrimArray arr n) n >> goArr arr sz (n + 1) 
