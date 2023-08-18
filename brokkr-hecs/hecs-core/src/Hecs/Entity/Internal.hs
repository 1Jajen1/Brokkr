{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Hecs.Entity.Internal (
  EntityId(..)
, EntitySparseSet(..)
, new
, allocateEntityId
, deAllocateEntityId
, isAlive
, insert, lookup
, Entity(..)
, EntityTag(..)
, Relation(..)
) where

import Prelude hiding (lookup)

import Control.Monad
import Control.Monad.Primitive

import Data.Bits
import Data.Bitfield.Internal
import Data.Primitive.Array
import Data.Primitive.ByteArray
import Data.Word

import Foreign.Storable

import GHC.Generics
import GHC.Exts
import GHC.IO

import Hecs.HashTable.HashKey

new :: IO (EntitySparseSet a)
new = do
  -- Allocate id storage with space for 256 entity ids...
  szRef <- newByteArray 8
  writeByteArray @Int szRef 0 0
  highestIdRef <- newByteArray 8
  writeByteArray @Int highestIdRef 0 0
  IO $ \s -> case newByteArray# initSz s of
    (# s1, arr #) -> case newMutVar# arr s1 of
      (# s2, arrRef #) -> case newByteArray# initIndSz s2 of
        (# s3, indArr #) -> case newMutVar# indArr s3 of
          (# s4, indArrRef #) -> case newArray# 256# (error "EntitySparseSet:new empty") s4 of
            (# s5, dataArr #) -> case newMutVar# dataArr s5 of
              (# s6, dataArrRef #) -> (# s6, EntitySparseSet szRef highestIdRef arrRef indArrRef dataArrRef #)
  where
    -- 8 bytes per entityid
    initSz = 8# *# 256#
    initIndSz = 4# *# 256#

allocateEntityId :: EntitySparseSet a -> IO EntityId
allocateEntityId f@(EntitySparseSet szRef highestIdRef arrRef indArrRef dataArrRef) = do
  sz <- readByteArray @Int szRef 0
  highestId <- readByteArray @Int highestIdRef 0
  !arr <- IO $ \s -> case readMutVar# arrRef s of (# s1, arr# #) -> (# s1, MutableByteArray arr# #)
  !indArr <- IO $ \s -> case readMutVar# indArrRef s of (# s1, indArr# #) -> (# s1, MutableByteArray indArr# #)
  -- Check if we have exhausted all freed ids
  if sz == highestId
    then do
      cap <- getSizeofMutableByteArray arr
      capInd <- getSizeofMutableByteArray indArr
      -- check if we have space in the id array left, if not just double it
      (arr'@(MutableByteArray arr'#), indArr'@(MutableByteArray indArr'#)) <- if sz >= unsafeShiftR cap 3
        then do
          arr' <- newByteArray (cap * 2)
          copyMutableByteArray arr 0 arr' 0 cap
          indArr' <- newByteArray (capInd * 2)
          copyMutableByteArray indArr 0 indArr' 0 capInd

          dataArr <- IO $ \s -> case readMutVar# dataArrRef s of (# s1, dArr #) -> (# s1, MutableArray dArr #)
          let dataArrCap = sizeofMutableArray dataArr
          dataArr'@(MutableArray dataArr'#) <- newArray (dataArrCap * 2) (error "EntitySparseSet:grow empty")
          copyMutableArray dataArr 0 dataArr' 0 dataArrCap
          IO $ \s -> (# writeMutVar# dataArrRef dataArr'# s, () #)

          pure (arr', indArr')
        else pure (arr, indArr)

      -- add our new id to the end of the array and also set its index mapping
      writeByteArray @Int arr' highestId highestId
      writeByteArray @Word32 indArr' highestId (fromIntegral highestId)
      -- increment the size and the highest ever id
      writeByteArray @Int szRef 0 (sz +  1)
      writeByteArray @Int highestIdRef 0 (highestId +  1)
      IO $ \s -> (# writeMutVar# arrRef arr'# s, () #)
      IO $ \s -> (# writeMutVar# indArrRef indArr'# s, () #)

      -- return the newly generated id
      pure $ EntityId $ Bitfield highestId
    else do
      -- reuse the last freed id
      -- For details as to why freed ids are at the end of the array in the first place check
      --  the deallocate method
      reusedId <- Bitfield @Int @Entity <$> readByteArray arr sz
      if get @"generation" reusedId == maxBound -- skip this entity forever. We cannot reliably reuse it anymore
        then do
          writeByteArray @Int szRef 0 (sz + 1)
          allocateEntityId f
        else do
          let newEid = pack $ Entity { eid = get @"eid" reusedId, generation = 1 + get @"generation" reusedId, pad = 0, tag = pack $ EntityTag False }
          writeByteArray arr sz (unwrap newEid)
          -- increment the size
          writeByteArray @Int szRef 0 (sz + 1)
          -- return the reused id but increment the generation
          pure $ EntityId newEid

-- TODO Handle double free!
deAllocateEntityId :: EntitySparseSet a -> EntityId -> IO (Maybe a)
deAllocateEntityId (EntitySparseSet szRef _ arrRef indArrRef dataArrRef) (EntityId eid) = do
  sz <- readByteArray @Int szRef 0

  indArr <- IO $ \s -> case readMutVar# indArrRef s of (# s1, indArr #) -> (# s1, MutableByteArray indArr #)
  
  -- lookup where in the id array the id to deallocate is
  eidInd <- readByteArray @Word32 indArr (fromIntegral $ get @"eid" eid)
  -- check if the entity is alive first
  if fromIntegral eidInd >= sz
    then pure Nothing
    else do
      arr <- IO $ \s -> case readMutVar# arrRef s of (# s1, arr #) -> (# s1, MutableByteArray arr #)
      mEid <- readByteArray @Int arr (fromIntegral eidInd)
      if mEid /= coerce eid
        then pure Nothing
        else do
          dataArr <- IO $ \s -> case readMutVar# dataArrRef s of (# s1, dataArr #) -> (# s1, MutableArray dataArr #)
          dataV <- readArray dataArr (fromIntegral eidInd)

          -- special case: Deallocating the last allocated id simply decreases the size counter
          unless (eidInd == fromIntegral (sz - 1)) $ do
            -- arr <- readIORef arrRef

            -- get the last allocated id (at the end of the array by construction. See allocate)
            lastActive <- readByteArray @Int arr (sz - 1)
            lastActiveV <- readArray dataArr (sz - 1)

            -- swap the id to deallocate with the last allocated id
            -- This now means the id we deallocate is at the end of all allocated ids (and before previously freed ids) 
            writeByteArray @Int arr (fromIntegral eidInd) lastActive 
            writeByteArray @Int arr (sz - 1) (unwrap eid)

            -- swap data array
            writeArray dataArr (fromIntegral eidInd) lastActiveV

            -- also swap the places in the index array to keep it up to date
            writeByteArray @Word32 indArr (fromIntegral $ get @"eid" (Bitfield @Int @Entity lastActive)) eidInd 
            writeByteArray @Word32 indArr (fromIntegral $ get @"eid" eid) (fromIntegral $ sz - 1)

          -- decrement the size and overwrite the last entry in the data array with an error to free the gc element
          writeArray dataArr (sz - 1) $ error "EntitySparseSet:deAllocate empty"
          writeByteArray @Int szRef 0 (sz - 1)
          pure $ Just dataV

isAlive :: EntitySparseSet a -> EntityId -> IO Bool
isAlive (EntitySparseSet szRef _ arrRef indArrRef _) (EntityId eid) = do
  sz <- readByteArray @Int szRef 0

  indArr <- IO $ \s -> case readMutVar# indArrRef s of (# s1, indArr #) -> (# s1, MutableByteArray indArr #)

  eidInd <- readByteArray @Word32 indArr (fromIntegral $ get @"eid" eid)
  -- check if the entity is alive first
  if fromIntegral eidInd >= sz
    then pure False
    else do
      arr <- IO $ \s -> case readMutVar# arrRef s of (# s1, arr #) -> (# s1, MutableByteArray arr #)
      mEid <- readByteArray @Int arr (fromIntegral eidInd)
      pure $ mEid == coerce eid

insert :: EntitySparseSet a -> EntityId -> a -> IO ()
insert (EntitySparseSet szRef _ arrRef indArrRef dataArrRef) (EntityId eid) v = do
  sz <- readByteArray @Int szRef 0

  indArr <- IO $ \s -> case readMutVar# indArrRef s of (# s1, indArr #) -> (# s1, MutableByteArray indArr #)

  eidInd <- readByteArray @Word32 indArr (fromIntegral $ get @"eid" eid)
  -- check if the entity is alive first
  if fromIntegral eidInd >= sz
    then pure ()
    else do
      arr <- IO $ \s -> case readMutVar# arrRef s of (# s1, arr #) -> (# s1, MutableByteArray arr #)
      mEid <- readByteArray @Int arr (fromIntegral eidInd)
      if mEid /= coerce eid
        then pure ()
        else do
          dataArr <- IO $ \s -> case readMutVar# dataArrRef s of (# s1, dataArr #) -> (# s1, MutableArray dataArr #)
          writeArray dataArr (fromIntegral eidInd) v

lookup :: EntitySparseSet a -> EntityId -> IO (Maybe a)
lookup (EntitySparseSet szRef _ arrRef indArrRef dataArrRef) (EntityId eid) = do
  sz <- readByteArray @Int szRef 0

  indArr <- IO $ \s -> case readMutVar# indArrRef s of (# s1, indArr #) -> (# s1, MutableByteArray indArr #)

  eidInd <- readByteArray @Word32 indArr (fromIntegral $ get @"eid" eid)
  -- check if the entity is alive first
  if fromIntegral eidInd >= sz
    then pure Nothing
    else do
      arr <- IO $ \s -> case readMutVar# arrRef s of (# s1, arr #) -> (# s1, MutableByteArray arr #)
      mEid <- readByteArray @Int arr (fromIntegral eidInd)
      if mEid /= coerce eid
        then pure Nothing
        else do
          dataArr <- IO $ \s -> case readMutVar# dataArrRef s of (# s1, dataArr #) -> (# s1, MutableArray dataArr #)
          Just <$> readArray dataArr (fromIntegral eidInd)

{- Note: Reusing entity ids

The data array stores all entity ids the following way: allocatedIds <> freedIds <> unusedSpace

After allocation the newest allocated id is always the last element of allocatedIds.
  - As long as either unusedSpace or freedIds are not empty the allocating a new id is either a simply write or even just
    increasing the size of allocatedIds and decreasing the size of the freedIds (both of which are derived from the same value, so this is
    really only incrementing sz by one)
  - If both unusedSpace and freedIds are empty, allocating first allocates a new backing array, copies data and then resumes with non-empty unusedSpace 

After deallocation the last deallocated id is always at the front of freedIds:
  - The only special case here is if the element is already at the back of the allocatedIds, this way we again only need to decrement sz
  - Otherwise we need to move our id back and then decrement sz. Moving back is achieved by swapping with the last element of allocatedIds

-}

data EntitySparseSet a where
  EntitySparseSet ::
       {-# UNPACK #-} !(MutableByteArray RealWorld) -- Int - current number of allocated ids
    -> {-# UNPACK #-} !(MutableByteArray RealWorld) -- Int - highest ever allocated id
    -> (MutVar# RealWorld (MutableByteArray# RealWorld)) -- 64 bits. array containing all allocated and freed ids
    -> (MutVar# RealWorld (MutableByteArray# RealWorld)) -- 32 bits. maps where in the above array an id is
    -> (MutVar# RealWorld (MutableArray# RealWorld a))
    -> EntitySparseSet a

-- | Unique identifier for entities
--
-- Internals: The entity id is a 64 bit bitfield and stores the following items:
-- * The entity id in 32 bits
-- * The generation in 16 bits
-- * A tag in 8 bits
--
-- This hints that 'EntityId's are reused. Specifically the 32 bit entity id is reused, but
-- the generation is incremented each time. This way even dead entity ids are unique.
-- This is often useful for network formats that benefit from having the entity ids be in
-- a smaller range and when keeping track of dead entity ids is not required.
newtype EntityId = EntityId { unEntityId :: Bitfield Int Entity }
  deriving stock Show
  deriving newtype (Eq, Storable)
  deriving HashKey via Int

-- | Datatype which encodes how 'Bitfield' accesses the 'EntityId'.
data Entity = Entity {
  eid        :: {-# UNPACK #-} !Word32
, generation :: {-# UNPACK #-} !Word16
, pad        :: {-# UNPACK #-} !Word8
, tag        :: {-# UNPACK #-} !(Bitfield Word8 EntityTag)
}
  deriving stock (Show, Generic)

-- | Relations are also entity ids, but stores data differently
--
-- Relations store only the entity id, not the generation as that is
-- redundant for components.
--
-- At runtime an entity id contains a 8 bit tag which indicates if it is
-- a relation or a normal entity id.
data Relation = Relation {
  first  :: {-# UNPACK #-} !Word32
, second :: {-# UNPACK #-} !Word24 -- TODO Isn't this quite unsafe if the second is a dynamic tag and needs more bits?!
, tag    :: {-# UNPACK #-} !(Bitfield Word8 EntityTag) 
}
  deriving stock Generic

newtype Word24 = Word24 Word
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral)
  deriving (HasFixedBitSize, AsRep r) via ViaIntegral 24 Word

data EntityTag = EntityTag {
  isRelation :: !Bool
}
  deriving stock (Show, Generic)
