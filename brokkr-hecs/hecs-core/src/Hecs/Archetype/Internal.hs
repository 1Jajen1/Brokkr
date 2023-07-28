{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Hecs.Archetype.Internal (
  ArchetypeEdge(..)
, Archetype(..)
, Columns#(..)
, readComponent
, writeComponent
, lookupComponent
, empty
, getEdge
, setEdge
, moveEntity
, ArchetypeTy
, addComponentType
, removeComponentType
, getTy
, createArchetype
, getColumnSizes
, addColumnSize
, iterateComponentIds
, unsfeGetColumn
, addEntity
, showSzs
, getNumEntities
, removeEntity
, removeColumnSize
, lookupWildcardB
, lookupWildcardL
, lookupWildcardR
) where

import Hecs.Component.Internal
import Hecs.Entity.Internal ( EntityId(..), Entity(..), EntityTag(..), Relation(..) )
import qualified Hecs.HashTable.Boxed as HTB
import Hecs.HashTable.HashKey

import GHC.Int
import GHC.Exts
import GHC.IO
import Foreign.Storable
import Unsafe.Coerce (unsafeCoerce)
import Data.Proxy
import Data.IORef
import Data.Bitfield

-- TODO This whole file needs a rework once done
-- TODO Add a custom allocator for the pinned aligned column arrays. This is to combat fragmentation

data ArchetypeEdge = ArchetypeEdge !(Maybe Archetype) !(Maybe Archetype)

-- TODO Make sum type for tags and other non-storage affecting data? I could also just share the column structure as it is entirely mutable
data Archetype = Archetype {
  edges        :: {-# UNPACK #-} !(IORef (HTB.HashTable (ComponentId Any) ArchetypeEdge))
, columns      :: {-# UNPACK #-} !Columns#
, componentTyB :: ComponentType#
, componentTyF :: ComponentType#
, componentTyT :: ComponentType#
}

instance Eq Archetype where
  l == r = getTy l == getTy r
  {-# INLINE (==) #-}

getTy :: Archetype -> ArchetypeTy
getTy Archetype{componentTyB, componentTyF, componentTyT} = ArchetypeTy componentTyB componentTyF componentTyT 

getNumEntities :: Archetype -> IO (Int, Int)
getNumEntities (Archetype{columns = Columns# szRef eidRef _ _ _}) = IO $ \s ->
  case readIntArray# szRef 0# s of
    (# s1, i #) -> case readMutVar# eidRef s1 of
      (# s2, arr #) -> (# s2, (I# i, I# (sizeofMutableByteArray# arr)) #) 

-- This is a monoid?
data ArchetypeTy = ArchetypeTy ComponentType# ComponentType# ComponentType#

iterateComponentIds :: ArchetypeTy -> (forall a . ComponentId a -> Int -> b -> IO b) -> IO b -> IO b
iterateComponentIds (ArchetypeTy tyB tyF tyT) f z = iterateTy tyB f z >>= \b -> iterateTy tyF f (pure b) >>= \b1 -> iterateTy tyT f (pure b1)
{-# INLINE iterateComponentIds #-}

iterateTy :: ComponentType# -> (ComponentId Any -> Int -> b -> IO b) -> IO b -> IO b
iterateTy (ComponentType# arr) f z = z >>= go 0# 
  where
    sz = uncheckedIShiftRL# (sizeofByteArray# arr) 3#
    go n b
      | isTrue# (n >=# sz) = pure b
      | otherwise = f (coerce $ I# (indexIntArray# arr n)) (I# n) b >>= go (n +# 1#)
{-# INLINE iterateTy #-}

instance Show ArchetypeTy where
  show (ArchetypeTy b u t) = "ArchetypeTy { boxed = " <> showTy b <> " unboxed = " <> showTy u <> " tag = " <> showTy t <> " }"

showTy :: ComponentType# -> String
showTy (ComponentType# arr) = "[" <> dropLast (go 0# "") <> "]"
  where
    dropLast [] = []
    dropLast [_] = []
    dropLast (x:y:xs) = x : dropLast (y:xs)
    sz = uncheckedIShiftRL# (sizeofByteArray# arr) 3#
    go n b | isTrue# (n >=# sz) = b
           | otherwise = show (I# (indexIntArray# arr n)) <> "," <> go (n +# 1#) b

instance Eq ArchetypeTy where
  ArchetypeTy lB lU lT == ArchetypeTy rB rU rT = eqComponentType lB rB && eqComponentType lU rU && eqComponentType lT rT
  {-# INLINE (==) #-}

instance HashKey ArchetypeTy where
  hashKey (ArchetypeTy b u t) = hashComponentType b <> hashComponentType u <> hashComponentType t
  {-# INLINE hashKey #-}
  
-- This does not need to be IO, many others don't need to either, move to arbitrary state and use ST?
addComponentType :: forall c ty . KnownComponentType ty => Proxy ty -> ArchetypeTy -> ComponentId c -> IO (ArchetypeTy, Int)
addComponentType ty (ArchetypeTy boxedTy unboxedTy tagTy) compId =
  branchCompType ty
    (IO $ \s -> case addComponent boxedTy compId s of
      (# s1, newBoxedTy, ind #) -> (# s1, (ArchetypeTy newBoxedTy unboxedTy tagTy, I# ind) #))
    (IO $ \s -> case addComponent unboxedTy compId s of
      (# s1, newUnboxedTy, ind #) -> (# s1, (ArchetypeTy boxedTy newUnboxedTy tagTy, I# ind) #))
    (IO $ \s -> case addComponent tagTy compId s of
      (# s1, newTagTy, ind #) -> (# s1, (ArchetypeTy boxedTy unboxedTy newTagTy, I# ind) #))
{-# SPECIALISE addComponentType :: forall c . Proxy Boxed -> ArchetypeTy -> ComponentId c -> IO (ArchetypeTy, Int) #-}
{-# SPECIALISE addComponentType :: forall c . Proxy Flat  -> ArchetypeTy -> ComponentId c -> IO (ArchetypeTy, Int) #-}
{-# SPECIALISE addComponentType :: forall c . Proxy Tag   -> ArchetypeTy -> ComponentId c -> IO (ArchetypeTy, Int) #-}

removeComponentType :: forall ty . KnownComponentType ty => Proxy ty -> ArchetypeTy -> Int -> IO ArchetypeTy
removeComponentType ty (ArchetypeTy boxedTy unboxedTy tagTy) col =
  branchCompType ty
    (IO $ \s -> case removeComponent boxedTy col s of
      (# s1, newBoxedTy #) -> (# s1, ArchetypeTy newBoxedTy unboxedTy tagTy #))
    (IO $ \s -> case removeComponent unboxedTy col s of
      (# s1, newUnboxedTy #) -> (# s1, ArchetypeTy boxedTy newUnboxedTy tagTy #))
    (IO $ \s -> case removeComponent tagTy col s of
      (# s1, newTagTy #) -> (# s1, ArchetypeTy boxedTy unboxedTy newTagTy #))
{-# SPECIALISE removeComponentType :: Proxy Boxed -> ArchetypeTy -> Int -> IO ArchetypeTy #-}
{-# SPECIALISE removeComponentType :: Proxy Flat  -> ArchetypeTy -> Int -> IO ArchetypeTy #-}
{-# SPECIALISE removeComponentType :: Proxy Tag   -> ArchetypeTy -> Int -> IO ArchetypeTy #-}

removeComponent :: ComponentType# -> Int -> State# RealWorld -> (# State# RealWorld, ComponentType# #)
removeComponent (ComponentType# arr) (I# col) s0 =
  case newByteArray# (bSz -# 8#) s0 of
    (# s1, newArr #) -> case copyByteArray# arr 0# newArr 0# (col *# 8#) s1 of
      s2 -> case copyByteArray# arr ((col *# 8#) +# 8#) newArr (col *# 8#) (bSz -# 8# -# (col *# 8#)) s2 of
        s3 -> case unsafeFreezeByteArray# newArr s3 of (# s4, bar #) -> (# s4, ComponentType# bar #)
  where
    bSz = sizeofByteArray# arr
{-# INLINE removeComponent #-}

newtype ComponentType# = ComponentType# ByteArray#

eqComponentType :: ComponentType# -> ComponentType# -> Bool
eqComponentType (ComponentType# l) (ComponentType# r)
  | isTrue# (sameByteArray# l r) = True
  | False <- isTrue# (nL ==# nR) = False
  | otherwise = EQ == compare (I# (compareByteArrays# l 0# r 0# nL)) 0
  where
    nL = sizeofByteArray# l
    nR = sizeofByteArray# r

hashComponentType :: ComponentType# -> HashFn
hashComponentType (ComponentType# arr) = HashFn $ \i -> go 0# i
  where
    sz = uncheckedIShiftRL# (sizeofByteArray# arr) 3#
    go n !h | isTrue# (n >=# sz) = h
            | otherwise = go (n +# 1#) (hashWithSalt h (hashKey (I# (indexIntArray# arr n))))
{-# INLINE hashComponentType #-}

addComponent :: ComponentType# -> ComponentId c -> State# RealWorld -> (# State# RealWorld, ComponentType#, Int# #)
addComponent (ComponentType# arr) (ComponentId (EntityId (Bitfield (I# i)))) s0 =
  case newByteArray# (bSz +# 8#) s0 of
    (# s1, mar #) -> case go mar 0# s1 of
      (# s2, ind #) -> case writeIntArray# mar ind i s2 of
        s3 -> case copyByteArray# arr (ind *# 8#) mar (8# *# (ind  +# 1#)) (bSz -# ind *# 8#) s3 of -- TODO Double check bounds
          s4 -> case unsafeFreezeByteArray# mar s4 of
            (# s5, newArr #) -> (# s5, ComponentType# newArr, ind #)
  where
    go mar n s | isTrue# (n >=# sz) = (# s, sz #)
               | isTrue# (i ># el) = case writeIntArray# mar n el s of s1 -> go mar (n +# 1#) s1
               | otherwise = (# s, n #)
      where el = indexIntArray# arr n
    sz = uncheckedIShiftRL# bSz 3#
    bSz = sizeofByteArray# arr
{-# INLINE addComponent #-}
  
-- Linear search for the component. Maybe instead use a (storable) hashtable? 
indexComponent# :: ComponentType# -> ComponentId c -> (Int# -> r) -> r -> r
indexComponent# (ComponentType# arr) (ComponentId (EntityId (Bitfield (I# i)))) s f = go 0#
  where
    sz = uncheckedIShiftRL# (sizeofByteArray# arr) 3#
    go n | isTrue# (n >=# sz) = f
    go n | isTrue# (indexIntArray# arr n ==# i) = s n
    go n = go (n +# 1#)
{-# INLINE indexComponent# #-}

indexWildcardB# :: ComponentType# -> (Int# -> r) -> r -> r
indexWildcardB# (ComponentType# arr) s f = go 0#
  where
    sz = uncheckedIShiftRL# (sizeofByteArray# arr) 3#
    go n
      | isTrue# (n >=# sz) = f
      | (Bitfield @Int @Entity (I# el)).tag.isRelation = s n
      | otherwise = go (n +# 1#)
      where
        el = indexIntArray# arr n
{-# INLINE indexWildcardB# #-}

indexWildcardL# :: ComponentType# -> ComponentId c -> (Int# -> r) -> r -> r
indexWildcardL# (ComponentType# arr) (ComponentId (EntityId (Bitfield (I# i)))) s f = go 0#
  where
    sz = uncheckedIShiftRL# (sizeofByteArray# arr) 3#
    go n
      | isTrue# (n >=# sz) = f
      | isTrue# (el ==# i) = s n
      | otherwise = go (n +# 1#)
      where
        el' = indexIntArray# arr n
        el = if (Bitfield @Int @Entity (I# el')).tag.isRelation
              then let !(I# sndId) = fromIntegral $ (Bitfield @Int @Relation (I# el')).second in sndId
              else el'
{-# INLINE indexWildcardL# #-}

indexWildCardR# :: ComponentType# -> ComponentId c -> (Int# -> r) -> r -> r
indexWildCardR# (ComponentType# arr) (ComponentId (EntityId (Bitfield (I# i)))) s f = go 0#
  where
    sz = uncheckedIShiftRL# (sizeofByteArray# arr) 3#
    go n
      | isTrue# (n >=# sz) = f
      | isTrue# (el ==# i) = s n
      | otherwise = go (n +# 1#)
      where
        el' = indexIntArray# arr n
        el = if (Bitfield @Int @Entity (I# el')).tag.isRelation
              then let !(I# firstId) = fromIntegral $ (Bitfield @Int @Relation (I# el')).first in firstId
              else el'
{-# INLINE indexWildCardR# #-}

numComponents :: ComponentType# -> Int#
numComponents (ComponentType# arr) = uncheckedIShiftRL# (sizeofByteArray# arr) 3#
{-# INLINE numComponents #-}

-- Holds either boxed Array# or unboxed (pinned) ByteArray#
-- rows - boxed arrs - unboxed arrs
-- All boxed and unboxed arrays have at least rows capacity, but will generally be larger
-- They are also all of the same capacity and are grown together
-- TODO I could use SmallMutableArray# for small capacities, just define some threshold for it and unsafeCoerce# it in?
data Columns# :: UnliftedType where
  Columns# ::
       MutableByteArray# RealWorld                                -- nr of used rows
    -> MutVar# RealWorld (MutableByteArray# RealWorld)            -- Entity ids
    -> SmallMutableArray# RealWorld (MutableArray# RealWorld Any) -- array of boxed component arrays
    -> ColumnSizes#                                               -- ByteSizes and alignment of the following components
    -> SmallMutableArray# RealWorld (MutableByteArray# RealWorld) -- array of unboxed component arrays
    -> Columns#

newtype ColumnSizes# = ColumnSizes# ByteArray#

showSzs :: ColumnSizes# -> String
showSzs (ColumnSizes# arr) = showTy (ComponentType# arr)

addColumnSize :: Int -> Int -> Int -> ColumnSizes# -> State# RealWorld -> (# State# RealWorld, ColumnSizes# #)
addColumnSize (I# at) (I# bSz) (I# bAl) (ColumnSizes# szs) s0 =
  case newByteArray# (sz +# 16#) s0 of
    (# s1, mar #) -> case writeIntArray# mar (at *# 2#) bSz s1 of
      s2 -> case writeIntArray# mar (at *# 2# +# 1#) bAl s2 of
        s3 -> case copyByteArray# szs 0# mar 0# (at *# 16#) s3 of -- TODO Double check bounds
          s4 -> case copyByteArray# szs (at *# 16#) mar (at *# 16# +# 16#) (sz -# at *# 16#) s4 of -- TODO Double check bounds
            s5 -> case unsafeFreezeByteArray# mar s5 of
              (# s6, newArr #) -> (# s6, ColumnSizes# newArr #)
  where
    sz = sizeofByteArray# szs

removeColumnSize :: Int -> ColumnSizes# -> State# RealWorld -> (# State# RealWorld, ColumnSizes# #)
removeColumnSize (I# at) (ColumnSizes# szs) s0 =
  case newByteArray# (sz -# 16#) s0 of
    (# s1, newArr #) -> case copyByteArray# szs 0# newArr 0# (at *# 16#) s1 of
      s2 -> case copyByteArray# szs ((at *# 16#) +# 16#) newArr (at *# 16#) (sz -# 16# -# (at *# 16#)) s2 of
        s3 -> case unsafeFreezeByteArray# newArr s3 of (# s4, bar #) -> (# s4, ColumnSizes# bar #)
  where
    sz = sizeofByteArray# szs

empty :: IO Archetype
empty = do
  edgesM <- HTB.new 8 -- TODO Inits
  edges <- newIORef edgesM
  IO $ \s0 ->
    case newArray# 0# (error "No") s0 of
      (# s1, arr #) -> case newSmallArray# 0# arr s1 of
        (# s2, mar1 #) -> case newByteArray# 0# s2 of
          (# s3, mbar #) -> case newSmallArray# 0# mbar s3 of
            (# s4, mar2 #) -> case unsafeFreezeByteArray# mbar s4 of
              (# s5, bar #) -> case newByteArray# 8# s5 of
                (# s6, szRef #) -> case writeIntArray# szRef 0# 0# s6 of
                  s7 -> case newByteArray# 8# s7 of
                    (# s8, eidArr #) -> case newMutVar# eidArr s8 of
                      (# s9, eidRef #) ->
                        (#
                          s9
                        , let columns      = Columns# szRef eidRef mar1 (ColumnSizes# bar) mar2
                              componentTyB = ComponentType# bar
                              componentTyF = ComponentType# bar
                              componentTyT = ComponentType# bar
                          in Archetype{..}
                        #)

-- Note: This performs no bounds checks. At this point we should have already checked if that entity and the component is in this table!
readComponent :: forall a . Component a => Proxy a -> Archetype -> Int -> Int -> IO a
readComponent p = backing p
  readBoxedComponent
  (\aty row col -> coerce $ readStorableComponent @(Value a) aty row col)
{-# INLINE readComponent #-}

readStorableComponent :: Storable a => Archetype -> Int -> Int -> IO a
readStorableComponent Archetype{columns = Columns# _ _ _ _ arrs} row (I# column) = IO $ \s ->
  case readSmallArray# arrs column s of
    (# s', colArr #) -> case peekElemOff (Ptr (mutableByteArrayContents# colArr)) row of
      IO f -> f s'
{-# INLINE readStorableComponent #-}

readBoxedComponent :: Archetype -> Int -> Int -> IO a
readBoxedComponent Archetype{columns = Columns# _ _ arrs _ _} (I# row) (I# column) = IO $ \s ->
  case readSmallArray# arrs column s of
    (# s', colArr #) -> case readArray# colArr row s' of
      (# s'', a #) -> (# s'', unsafeCoerce a #)

writeComponent :: forall a . Component a => Proxy a -> Archetype -> Int -> Int -> a -> IO ()
writeComponent p = backing p
  writeBoxedComponent
  (\aty row col el -> writeStorableComponent @(Value a) aty row col (coerce el))
{-# INLINE writeComponent #-}

writeStorableComponent :: forall a . Storable a => Archetype -> Int -> Int -> a -> IO ()
writeStorableComponent Archetype{columns = Columns# _ _ _ _ arrs} row (I# column) el = IO $ \s0 ->
  case readSmallArray# arrs column s0 of
    (# s1, colArr #) -> case pokeElemOff (Ptr (mutableByteArrayContents# colArr)) row el of
      IO f -> f s1
{-# INLINE writeStorableComponent #-}

writeBoxedComponent :: Archetype -> Int -> Int -> a -> IO ()
writeBoxedComponent Archetype{columns = Columns# _ _ arrs _ _} (I# row) (I# column) el = IO $ \s0 ->
  case readSmallArray# arrs column s0 of
    (# s1, colArr #) -> case writeArray# colArr row (unsafeCoerce el) s1 of
      s2 -> (# s2, () #)

lookupComponent :: forall c ty r . KnownComponentType ty => Proxy ty -> Archetype -> ComponentId c -> (Int -> r) -> r -> r
lookupComponent ty = branchCompType ty
  (\Archetype{componentTyB} compId s -> indexComponent# componentTyB compId (\i -> s (I# i)))
  (\Archetype{componentTyF} compId s -> indexComponent# componentTyF compId (\i -> s (I# i)))
  (\Archetype{componentTyT} compId s -> indexComponent# componentTyT compId (\i -> s (I# i)))
{-# INLINE lookupComponent #-}

lookupWildcardB :: forall ty r . KnownComponentType ty => Proxy ty -> Archetype -> (Int -> r) -> r -> r
lookupWildcardB ty = branchCompType ty
  (\Archetype{componentTyB} s -> indexWildcardB# componentTyB (\i -> s (I# i)))
  (\Archetype{componentTyF} s -> indexWildcardB# componentTyF (\i -> s (I# i)))
  (\Archetype{componentTyT} s -> indexWildcardB# componentTyT (\i -> s (I# i)))
{-# INLINE lookupWildcardB #-}

lookupWildcardL :: forall c ty r . KnownComponentType ty => Proxy ty -> Archetype -> ComponentId c -> (Int -> r) -> r -> r
lookupWildcardL ty = branchCompType ty
  (\Archetype{componentTyB} compId s -> indexWildcardL# componentTyB compId (\i -> s (I# i)))
  (\Archetype{componentTyF} compId s -> indexWildcardL# componentTyF compId (\i -> s (I# i)))
  (\Archetype{componentTyT} compId s -> indexWildcardL# componentTyT compId (\i -> s (I# i)))
{-# INLINE lookupWildcardL #-}

lookupWildcardR :: forall c ty r . KnownComponentType ty => Proxy ty -> Archetype -> ComponentId c -> (Int -> r) -> r -> r
lookupWildcardR ty = branchCompType ty
  (\Archetype{componentTyB} compId s -> indexWildCardR# componentTyB compId (\i -> s (I# i)))
  (\Archetype{componentTyF} compId s -> indexWildCardR# componentTyF compId (\i -> s (I# i)))
  (\Archetype{componentTyT} compId s -> indexWildCardR# componentTyT compId (\i -> s (I# i)))
{-# INLINE lookupWildcardR #-}

unsfeGetColumn :: forall c ty . KnownComponentType ty => Proxy ty -> Archetype -> Int -> IO (Column ty c)
unsfeGetColumn ty (Archetype{columns = Columns# _ _ boxed _ unboxed}) (I# col) = branchCompType ty
  (IO $ \s -> case readSmallArray# boxed col s of (# s1, arr #) -> (# s1, ColumnBoxed (unsafeCoerce# arr) #))
  (IO $ \s -> case readSmallArray# unboxed col s of (# s1, arr #) -> (# s1, ColumnFlat arr #))
  (error "Tried to get a column for a tag")

grow :: Columns# -> State# RealWorld -> State# RealWorld
grow (Columns# _ eids boxed (ColumnSizes# szs) unboxed) s = case copyUnboxed 0# (copyBoxed 0# s) of
  s1 -> case readMutVar# eids s1 of
    (# s2, eidarr #) -> case newByteArray# (2# *# sizeofMutableByteArray# eidarr) s2 of
      (# s3, newarr #) -> case copyMutableByteArray# eidarr 0# newarr 0# (sizeofMutableByteArray# eidarr) s3 of
        s4 -> writeMutVar# eids newarr s4
  where
    copyUnboxed n s0 | isTrue# (n >=# numUnboxed) = s0
    copyUnboxed n s0 =
      case readSmallArray# unboxed n s0 of
        (# s1, arr #) -> case newAlignedPinnedByteArray# (2# *# sizeofMutableByteArray# arr) bAl s1 of
          (# s2, newArr #) -> case copyMutableByteArray# arr 0# newArr 0# (sizeofMutableByteArray# arr) s2 of
            s3 -> case writeSmallArray# unboxed n newArr s3 of
              s4 -> copyUnboxed (n +# 1#) s4
      where bAl = indexIntArray# szs ((n *# 2#) +# 1#)
    copyBoxed n s0 | isTrue# (n >=# numBoxed) = s0
    copyBoxed n s0 =
      case readSmallArray# boxed n s0 of
        (# s1, arr #) -> case newArray# (2# *# sizeofMutableArray# arr) (error "Hecs.Archetype.Internal:grow Placeholder") s1 of
          (# s2, newArr #) -> case copyMutableArray# arr 0# newArr 0# (sizeofMutableArray# arr) s2 of
            s3 -> case writeSmallArray# boxed n newArr s3 of
              s4 -> copyBoxed (n +# 1#) s4
    numBoxed = sizeofSmallMutableArray# boxed
    numUnboxed = sizeofSmallMutableArray# unboxed

getEdge :: Archetype -> ComponentId c -> IO ArchetypeEdge
getEdge (Archetype{edges}) compId = readIORef edges >>= \em -> HTB.lookup em (coerce compId) pure (pure $ ArchetypeEdge Nothing Nothing)

setEdge :: Archetype -> ComponentId c -> ArchetypeEdge -> IO ()
setEdge (Archetype{edges}) compId edge = readIORef edges >>= \em -> HTB.insert em (coerce compId) edge >>= writeIORef edges

getColumnSizes :: Archetype -> ColumnSizes#
getColumnSizes Archetype{columns = Columns# _ _ _ szs _} = szs

-- It is assumed that the archetype does not already have the component
createArchetype :: ArchetypeTy -> ColumnSizes# -> IO Archetype
createArchetype (ArchetypeTy boxedTy unboxedTy tagTy) szs = do
  -- traceIO $ "New Archetype with type: " <> show (ArchetypeTy boxedTy unboxedTy tagTy)
  edgesM' <- HTB.new 8 -- TODO Inits
  edges <- newIORef edgesM'
  IO $ \s0 -> case newColumns initSz numBoxed numUnboxed szs s0 of
    (# s1, cols #) -> (# s1, Archetype edges cols boxedTy unboxedTy tagTy #)
  where
    initSz = 8# -- TODO Inits
    numBoxed = numComponents boxedTy
    numUnboxed = numComponents unboxedTy

-- Only for the empty archetype really ...
addEntity :: Archetype -> EntityId -> IO Int
addEntity Archetype{columns = c@(Columns# szRef eidRefs _ _ _)} (EntityId (Bitfield (I# eid))) = IO $ \s0 ->
  case readIntArray# szRef 0# s0 of
    (# s1, sz #) -> case readMutVar# eidRefs s1 of
      (# s2, eidArr #) -> case (if isTrue# (sz >=# uncheckedIShiftRL# (sizeofMutableByteArray# eidArr) 3#)
        then grow c s2
        else s2) of
          s3 -> case readMutVar# eidRefs s3 of
            (# s4, actualEidArr #) -> case writeIntArray# actualEidArr sz eid s4 of
              s5 -> case writeIntArray# szRef 0# (sz +# 1#) s5 of
                s6 -> (# s6, I# sz #)

removeEntity :: Archetype -> Int -> IO EntityId
removeEntity aty@Archetype{columns = Columns# szRef eidRefs _ _ _} (I# row) = IO $ \s0 ->
  case readIntArray# szRef 0# s0 of
    (# s1, sz #) -> case readMutVar# eidRefs s1 of
      (# s2, eidArr #) -> case readIntArray# eidArr (sz -# 1#) s2 of
        (# s3, moved #) -> case writeIntArray# eidArr row moved s3 of
          s4 -> case writeIntArray# szRef 0# (sz -# 1#) s4 of
            s5 -> case removeEntity' aty (sz -# 1#) row s5 of
              s6 -> (# s6, coerce (I# moved) #)

removeEntity' :: Archetype -> Int# -> Int# -> State# RealWorld -> State# RealWorld  
removeEntity' aty lastI row s0 = copyUnboxed 0# (copyBoxed 0# s0)
  where
    -- TODO Don't copy around ourselves (row == srcLast) in boxed components because that also leaves gc refs...
    copyBoxed n s
      | isTrue# (n >=# numBoxed) = s
      | otherwise =
        case readSmallArray# boxed n s of
          (# s1, srcArr #) -> case readArray# srcArr lastI s1 of
              (# s2, lastEl #) -> copyBoxed (n +# 1#)
                ( writeArray# srcArr lastI (error "Hecs.Internal.Archetype.removeEntity' placeholder")
                  (writeArray# srcArr row lastEl s2)
                )

    copyUnboxed n s
      | isTrue# (n >=# numUnboxed) = s
      | otherwise =
        case readSmallArray# unboxed n s of
          (# s1, srcArr #) -> copyUnboxed (n +# 1#)
            (copyMutableByteArray# srcArr (lastI *# bSz) srcArr (row *# bSz) bSz s1)
      where
        bSz = indexIntArray# szs (n *# 2#)
    !(Columns# _ _ boxed (ColumnSizes# szs) unboxed) = columns aty
    numBoxed = sizeofSmallMutableArray# boxed
    numUnboxed = sizeofSmallMutableArray# unboxed

moveEntity :: Archetype -> Int -> Int -> Archetype -> IO (Int, EntityId)
moveEntity srcAty (I# row) (I# skipColumn) dstAty = IO $ \s ->
  case readIntArray# dstSzRef 0# s of
    (# s1, dstSz #) -> case readCap s1 of
      (# s2, dstCap #) -> case (if isTrue# (dstSz >=# dstCap)
        then grow (columns dstAty) s2
        else s2) of
          s3 -> case readIntArray# srcSzRef 0# s3 of
            (# s4, srcSz #) -> case writeIntArray# dstSzRef 0# (dstSz +# 1#) s4 of
              s5 -> case writeIntArray# srcSzRef 0# (srcSz -# 1#) s5 of
                s6 -> case readMutVar# srcEids s6 of
                  (# s7, srcEidArr #) -> case readMutVar# dstEids s7 of
                    (# s8, dstEidArr #) -> case copyMutableByteArray# srcEidArr (row *# 8#) dstEidArr (dstSz *# 8#) 8# s8 of
                      s9 -> case copyMutableByteArray# srcEidArr (8# *# (srcSz -# 1#)) srcEidArr (row *# 8#) 8# s9 of
                        s10 -> case readIntArray# srcEidArr row s10 of
                          (# s11, movedEid #) ->
                            (# moveEntity' srcAty (srcSz -# 1#) row skipColumn dstAty dstSz s11, (I# dstSz, EntityId $ Bitfield (I# movedEid)) #)
  where
    !(Columns# srcSzRef srcEids _ _ _) = columns srcAty
    !(Columns# dstSzRef dstEids _ _ _) = columns dstAty
    readCap s = case readMutVar# dstEids s of (# s1, arr #) -> (# s1, uncheckedIShiftRL# (sizeofMutableByteArray# arr) 3# #)

moveEntity' :: Archetype -> Int# -> Int# -> Int# -> Archetype -> Int# -> State# RealWorld -> State# RealWorld
moveEntity' srcAty srcLast row skipColumn dstAty newRow s0 = copyUnboxed 0# 0# (copyBoxed 0# 0# s0)
  where
    copyBoxed n m s
      | isTrue# (n >=# srcNumBoxed) = s
      | isTrue# (m ==# skipColumn) && changedBoxed && isAdd     = copyBoxed n (m +# 1#) s
      | isTrue# (n ==# skipColumn) && changedBoxed && not isAdd = copyBoxed (n +# 1#) m s
      | otherwise =
        case readSmallArray# srcBoxed n s of
          (# s1, srcArr #) -> case readArray# srcArr row s1 of
            (# s2, el #) -> case readArray# srcArr srcLast s2 of
              (# s3, lastEl #) -> case readSmallArray# dstBoxed m s3 of
                (# s4, dstArr #) -> copyBoxed (n +# 1#) (m +# 1#)
                  ( writeArray# srcArr row lastEl
                    ( writeArray# srcArr srcLast (error "Hecs.Internal.Archetype.removeEntity' placeholder")
                      (writeArray# dstArr newRow el s4)
                    )
                  )

    copyUnboxed n m s
      | isTrue# (n >=# srcNumUnboxed) = s
      | isTrue# (m ==# skipColumn) && changedUnboxed && isAdd     = copyUnboxed n (m +# 1#) s
      | isTrue# (n ==# skipColumn) && changedUnboxed && not isAdd = copyUnboxed (n +# 1#) m s
      | otherwise =
        case readSmallArray# srcUnboxed n s of
          (# s1, srcArr #) -> case readSmallArray# dstUnboxed m s1 of
              (# s3, dstArr #) ->
                copyUnboxed (n +# 1#) (m +# 1#)
                  ( copyMutableByteArray# srcArr (srcLast *# bSz) srcArr (row *# bSz) bSz
                    (copyMutableByteArray# srcArr (row *# bSz) dstArr (newRow *# bSz) bSz s3)
                  )
      where
        bSz = indexIntArray# srcSzs (n *# 2#)

    !(Columns# _ _ srcBoxed (ColumnSizes# srcSzs) srcUnboxed) = columns srcAty
    !(Columns# _ _ dstBoxed _ dstUnboxed) = columns dstAty
    srcNumBoxed = sizeofSmallMutableArray# srcBoxed
    dstNumBoxed = sizeofSmallMutableArray# dstBoxed
    srcNumUnboxed = sizeofSmallMutableArray# srcUnboxed
    dstNumUnboxed = sizeofSmallMutableArray# dstUnboxed
    changedBoxed = not $ isTrue# (srcNumBoxed ==# dstNumBoxed)
    changedUnboxed = not $ isTrue# (srcNumUnboxed ==# dstNumUnboxed)
    isAdd = isTrue# (srcNumBoxed <# dstNumBoxed) || isTrue# (srcNumUnboxed <# dstNumUnboxed)

newColumns :: Int# -> Int# -> Int# -> ColumnSizes# -> State# RealWorld -> (# State# RealWorld, Columns# #) 
newColumns initCap numBoxed numUnboxed (ColumnSizes# szs) s0 =
  case newByteArray# 8# s0 of
    (# s1, szRef #) -> case newArray# initCap (error "Hecs.Archetype.Internal:newColumns placeholder") s1 of
      (# s2, arr #) -> case newSmallArray# numBoxed arr s2 of
        (# s3, boxed #) -> case fillBoxed boxed 0# s3 of
          s4 -> case newByteArray# 0# s4 of
            (# s5, mar #) -> case newSmallArray# numUnboxed mar s5 of
              (# s6, unboxed #) -> case fillUnboxed unboxed 0# s6 of
                s7 -> case newByteArray# (initCap *# 8#) s7 of
                  (# s8, eidArr #) -> case newMutVar# eidArr s8 of
                    (# s9, eidRef #) -> case writeIntArray# szRef 0# 0# s9 of
                      s10 -> (# s10, Columns# szRef eidRef boxed (ColumnSizes# szs) unboxed #)
  where
    fillBoxed _ n s | isTrue# (n >=# numBoxed) = s
    fillBoxed sarr n s =
      case newArray# initCap (error "Hecs.Archetype.Internal:moveEntity placeholder") s of
        (# s1, arr #) -> fillBoxed sarr (n +# 1#) (writeSmallArray# sarr n arr s1)
    fillUnboxed _ n s | isTrue# (n >=# numUnboxed) = s
    fillUnboxed sarr n s =
      case newAlignedPinnedByteArray# (initCap *# bSz) bAl s of
        (# s1, arr #) -> fillUnboxed sarr (n +# 1#) (writeSmallArray# sarr n arr s1)
      where
        bSz = indexIntArray# szs (n *# 2#)
        bAl = indexIntArray# szs ((n *# 2#) +# 1#)
