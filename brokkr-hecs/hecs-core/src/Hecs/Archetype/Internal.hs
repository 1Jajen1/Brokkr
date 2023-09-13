{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UnliftedDatatypes #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Hecs.Archetype.Internal (
  Archetype(..)
, Archetype#(..)
, getFlags
, ArchetypeFlags(..)
, getNrRows
, getEntityColumn
, getColumn, getBitSet
, lookupColumn, lookupRelationColumns
, readComponent, writeComponent
, isComponentEnabled
, enableComponent, disableComponent
, addEntity, removeEntity
, ArchetypeEdge(..)
, getEdge, setEdge
, ArchetypeMove(..)
, moveTo
, SizesAndAlignment
, sizesAndAlignment
, addComponentSizes, removeComponentSizes
, newWithType
, newEmpty
) where

import Brokkr.HashTable qualified as HT

import Control.Monad.Primitive
import Control.Monad.ST.Strict (runST)

import Data.Bitfield
import Data.Int
import Data.Primitive
import Data.Primitive.PrimVar
import Data.Proxy
import Data.Word

import Foreign.Storable (Storable)
import Foreign.Storable qualified as Storable

import GHC.Exts
import GHC.Generics (Generic)

import Hecs.Archetype.Storage (Storage(..), SizesAndAlignment(..))
import Hecs.Archetype.Storage qualified as Storage
import Hecs.Archetype.Type (ArchetypeType(..), linearSearch)
import Hecs.Archetype.Type qualified as Type

import Hecs.Component.Column
import Hecs.Component.Internal
import Hecs.Component.Relation

import Hecs.Entity.Internal (EntityId(..))

-- TODO Make unlifted
data Archetype = Archetype Archetype#

data Archetype# :: UnliftedType where
  Archetype# :: {
      ty       ::                !ArchetypeType
    , size     :: {-# UNPACK #-} !(PrimVar RealWorld Int)
    , flags    :: {-# UNPACK #-} !(PrimVar RealWorld (Bitfield Word32 ArchetypeFlags))
    , entities ::                 (MutVar# RealWorld (MutableByteArray# RealWorld))
    , boxed    :: {-# UNPACK #-} !(Storage Boxed RealWorld)
    , flat     :: {-# UNPACK #-} !(Storage Flat  RealWorld)
    , tag      :: {-# UNPACK #-} !(Storage Tag   RealWorld)
    , edges    :: {-# UNPACK #-} !(HT.HashTable' HT.Storable HT.Boxed RealWorld IdInt ArchetypeEdge)
    } -> Archetype#

-- Bad. Add to Bitfield!
deriving newtype instance Prim rep => Prim (Bitfield rep a)

data ArchetypeFlags = ArchetypeFlags {
  hasBitSets :: Bool
}
  deriving stock Generic

getFlags :: Archetype -> IO (Bitfield Word32 ArchetypeFlags)
getFlags (Archetype Archetype#{flags}) = readPrimVar flags

newtype IdInt = IdInt Int
  deriving newtype (Eq, Storable)

instance HT.Hash IdInt where hash (IdInt x) = HT.HashFn (const x)

instance Eq Archetype where
  (Archetype Archetype#{ty = lTy}) == (Archetype Archetype#{ty = rTy}) = lTy == rTy

data ArchetypeEdge
  = ArchetypeEdge
    {-# UNPACK #-} !Archetype -- destination
    {-# UNPACK #-} !Int       -- column

newEmpty :: IO Archetype
newEmpty = newWithType (ArchetypeType 0 mempty mempty mempty mempty) (SizesAndAlignment mempty)

getNrRows :: Archetype -> IO Int
{-# INLINE getNrRows #-}
getNrRows (Archetype Archetype#{size}) = readPrimVar size

getColumn :: forall c r . Component c => Archetype -> Int -> (Column (ComponentKindFor c) RealWorld c -> IO r) -> IO r
{-# INLINE getColumn #-}
getColumn (Archetype Archetype#{boxed,flat,tag}) = backing @_ @c
  (Storage.getColumn boxed)
  (Storage.getColumn flat )
  (Storage.getColumn tag  )

getEntityColumn :: Archetype -> (Column Flat RealWorld EntityId -> IO r) -> IO r
{-# INLINE getEntityColumn #-}
getEntityColumn (Archetype Archetype#{entities}) cont = do
  col <- primitive $ \s -> case readMutVar# entities s of (# s', arr #) -> (# s', FlatColumn $ MutableByteArray arr #)
  cont col

getBitSet :: forall c r . KnownComponentKind c => Archetype -> Int -> (MutablePrimArray RealWorld Word64 -> IO r) -> IO r -> IO r
{-# INLINE getBitSet #-}
getBitSet (Archetype Archetype#{boxed,flat,tag}) col = oneShot $ \onSucc -> oneShot $ \onFail -> branchKind @c
  (Storage.getBitSet boxed)
  (Storage.getBitSet flat )
  (Storage.getBitSet tag  )
  col onSucc onFail

lookupColumn :: forall c r . Component c => Archetype -> ComponentId c -> (Int -> r) -> r -> r
{-# INLINE lookupColumn #-}
lookupColumn (Archetype Archetype#{ty}) cid onSucc onFail = Type.findComponent ty cid onSucc onFail

lookupRelationColumns :: Archetype -> ComponentId (Rel a b) -> (Int -> Int -> r -> r) -> r -> r
{-# INLINE lookupRelationColumns #-}
lookupRelationColumns (Archetype Archetype#{ty}) cid onSucc onFail = Type.findRelationMatches ty cid onSucc onFail

addEntity :: Archetype -> EntityId -> IO Int
addEntity (Archetype Archetype#{size,entities,boxed,flat,tag}) entity = do
  sz <- readPrimVar size
  eids0 <- primitive $ \s -> case readMutVar# entities s of (# s', arr #) -> (# s', MutablePrimArray arr #)
  cap <- getSizeofMutablePrimArray eids0
  eids <- if cap > sz
    then pure eids0
    else do
      let newSz = growFactor cap
      newPrimArr@(MutablePrimArray new') <- resizeMutablePrimArray eids0 newSz
      primitive $ \s -> (# writeMutVar# entities new' s, () #)
      Storage.growStorage boxed newSz
      Storage.growStorage flat  newSz
      Storage.growStorage tag   newSz
      pure newPrimArr
  writePrimArray @Int eids sz $ coerce entity
  writePrimVar size $ sz + 1
  pure sz

-- TODO Remove from storage too!
removeEntity :: Archetype -> Int -> IO EntityId
removeEntity (Archetype Archetype#{..}) row = do
  sz <- readPrimVar size
  let last = sz - 1
  eids <- primitive $ \s -> case readMutVar# entities s of (# s', arr #) -> (# s', MutablePrimArray arr #)
  lastEid :: Int <- readPrimArray eids last
  writePrimArray eids row lastEid
  writePrimVar size last
  pure $ coerce lastEid

growFactor :: Int -> Int
-- 1.5 growth factor
growFactor i = (i * 3) `quot` 2

getEdge :: Archetype -> ComponentId c -> (ArchetypeEdge -> IO r) -> IO r -> IO r
{-# INLINE getEdge #-}
getEdge (Archetype Archetype#{edges}) cid = HT.lookup edges (coerce cid)

setEdge :: Archetype -> ComponentId c -> ArchetypeEdge -> IO ()
setEdge (Archetype Archetype#{edges}) cid edge = HT.insert edges (coerce cid) edge

data ArchetypeMove
  = ArchetypeMove
    {-# UNPACK #-} !Int      -- new row in the destination archetype
    {-# UNPACK #-} !EntityId -- the entity we swapped with in the origin archetype

moveTo :: Archetype -> Archetype -> Int -> IO ArchetypeMove
moveTo (Archetype origin) (Archetype destination@Archetype#{flags}) !row = do
  -- calculate the move masks
  let (!boxedMask, !flatMask, !tagMask) = mkMoveMasks (ty origin) (ty destination)

  -- remove the entity from the current archetype by swapping with the last
  szOrigin <- readPrimVar (size origin)
  let lastOrigin = szOrigin - 1
  writePrimVar (size origin) lastOrigin
  
  eidsOrigin <- primitive $ \s -> case readMutVar# (entities origin) s of (# s', arr #) -> (# s', MutablePrimArray arr #)
  toMove  :: Int <- readPrimArray eidsOrigin row
  lastEid :: Int <- readPrimArray eidsOrigin lastOrigin
  writePrimArray eidsOrigin row lastEid

  -- addEntity also takes care of growing all arrays if necessary
  newRow <- addEntity (Archetype destination) (coerce toMove)

  eidsDestination <- primitive $ \s -> case readMutVar# (entities destination) s of (# s', arr #) -> (# s', MutablePrimArray arr #)
  toCap <- getSizeofMutablePrimArray @_ @Int eidsDestination

  -- move components
  let newBitSetCallback = do
        dstFlags <- readPrimVar flags
        writePrimVar flags $ set @"hasBitSets" dstFlags True
  Storage.moveTo (boxed origin) (boxed destination) lastOrigin row newRow toCap boxedMask newBitSetCallback
  Storage.moveTo (flat  origin) (flat  destination) lastOrigin row newRow toCap flatMask newBitSetCallback
  Storage.moveTo (tag   origin) (tag   destination) lastOrigin row newRow toCap tagMask newBitSetCallback

  pure $ ArchetypeMove newRow (coerce lastEid)
  where
    mkMoveMasks (ArchetypeType _ oB oF oT _) (ArchetypeType _ dB dF dT _) = (mkMoveMask oB dB, mkMoveMask oF dF, mkMoveMask oT dT)
    mkMoveMask oArr dArr = runST $ do
      let sz = sizeofPrimArray oArr
      mar <- newPrimArray sz
      let goMoveMask n
            | n >= sz   = pure ()
            | otherwise = do
              let src = indexPrimArray oArr n
              case linearSearch dArr src of
                (# n# | #) -> writePrimArray mar n (I# n#)
                (# | _ #)  -> writePrimArray mar n (-1)
      goMoveMask 0
      unsafeFreezePrimArray mar

sizesAndAlignment :: Archetype -> SizesAndAlignment
sizesAndAlignment (Archetype Archetype#{flat = FlatStorage sz _ _}) = sz

addComponentSizes :: forall c . Storable c => Proxy c -> SizesAndAlignment -> SizesAndAlignment
addComponentSizes _ (SizesAndAlignment bs) = runST $ do
  let sz = sizeofByteArray bs
  mba <- newByteArray (sz + 2)
  copyByteArray mba 0 bs 0 sz
  writeByteArray @Int8 mba sz       (fromIntegral $ Storable.sizeOf    (undefined :: c))
  writeByteArray @Int8 mba (sz + 1) (fromIntegral $ Storable.alignment (undefined :: c))
  SizesAndAlignment <$> unsafeFreezeByteArray mba

removeComponentSizes :: Int -> SizesAndAlignment -> SizesAndAlignment
removeComponentSizes col (SizesAndAlignment bs) = runST $ do
  let sz = sizeofByteArray bs
  mba <- newByteArray (sz + 2)
  copyByteArray mba 0 bs 0 (col * 2)
  copyByteArray mba col bs (col * 2 + 2) (sz - col * 2 - 2)
  SizesAndAlignment <$> unsafeFreezeByteArray mba

newWithType :: ArchetypeType -> SizesAndAlignment -> IO Archetype
newWithType ty@(ArchetypeType _ b f t _) sizesAndAlign = do
  size <- newPrimVar 0
  flags <- newPrimVar $ pack ArchetypeFlags { hasBitSets = False }
  edges <- HT.new 4 0 0.75 -- We use a precomputed hash, also nothing user defined, so we need no salt
  boxed <- Storage.newBoxed (sizeofPrimArray b) initSize
  flat  <- Storage.newFlat  (sizeofPrimArray f) sizesAndAlign initSize
  tag   <- Storage.newTag   (sizeofPrimArray t) initSize
  MutablePrimArray eidArr <- newPrimArray @_ @Int initSize
  primitive $ \s ->
    case newMutVar# eidArr s of
      (# s', entities #) -> (# s', Archetype Archetype#{..} #)
  where
    initSize = 4

readComponent :: forall c r . (Component c, Coercible (ComponentValueFor c) c) => Archetype -> Int -> Int -> (c -> IO r) -> IO r
{-# INLINE readComponent #-}
readComponent (Archetype Archetype#{boxed,flat,tag}) =
  backing @_ @c
    (Storage.readStorage boxed)
    (Storage.readStorage flat )
    (Storage.readStorage tag  )

writeComponent :: forall c . (Component c, Coercible (ComponentValueFor c) c) => Archetype -> Int -> Int -> c -> IO ()
{-# INLINE writeComponent #-}
writeComponent (Archetype Archetype#{boxed,flat,tag}) =
  backing @_ @c
    (Storage.writeStorage boxed)
    (Storage.writeStorage flat )
    (Storage.writeStorage tag  )

isComponentEnabled :: forall c . KnownComponentKind c => Proxy c -> Archetype -> Int -> Int -> IO Bool
isComponentEnabled _ (Archetype Archetype#{flags, boxed,flat,tag}) row col = do
  archetypeFlags <- readPrimVar flags
  if get @"hasBitSets" archetypeFlags
    then do
      branchKind @c
        (Storage.isEnabled boxed)
        (Storage.isEnabled flat )
        (Storage.isEnabled tag  )
        row col
    else pure False

enableComponent :: forall c . KnownComponentKind c => Proxy c -> Archetype -> Int -> Int -> IO ()
enableComponent _ (Archetype Archetype#{flags,boxed,flat,tag}) row col = do
  archetypeFlags <- readPrimVar flags
  if get @"hasBitSets" archetypeFlags
    then do
      branchKind @c
        (Storage.enableComponent boxed)
        (Storage.enableComponent flat )
        (Storage.enableComponent tag  )
        row col
    else pure ()

disableComponent :: forall c . KnownComponentKind c => Proxy c -> Archetype -> Int -> Int -> IO ()
disableComponent _ (Archetype Archetype#{flags,entities,boxed,flat,tag}) row col = do
  arr <- primitive $ \s -> case readMutVar# entities s of (# s', arr #) -> (# s', MutablePrimArray @_ @Int arr #)
  oldFlags <- readPrimVar flags
  writePrimVar flags $ set @"hasBitSets" oldFlags True

  cap <- getSizeofMutablePrimArray arr
  branchKind @c
    (Storage.disableComponent boxed row col cap)
    (Storage.disableComponent flat  row col cap)
    (Storage.disableComponent tag   row col cap)
