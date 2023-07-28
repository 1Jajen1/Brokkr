{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
module Hecs.Entity.Internal (
  EntityId(..)
, FreshEntityId(..)
, new
, allocateEntityId
, deAllocateEntityId
, Entity(..)
, EntityTag(..)
, Relation(..)
) where

import Control.Monad
import Control.Monad.Primitive

import Data.Bits
import Data.Bitfield.Internal
import Data.Word
import Data.Primitive.ByteArray

import Foreign.Storable

import GHC.Generics

import Hecs.HashTable.HashKey

new :: IO FreshEntityId
new = do
  -- Allocate id storage with space for 256 entity ids...
  arr <- newByteArray initSz
  indArr <- newByteArray initIndSz
  pure $ FreshEntityId 0 0 arr indArr
  where
    -- 8 bytes per entityid
    initSz = 8 * 256
    initIndSz = 4 * 256

allocateEntityId :: FreshEntityId -> IO (FreshEntityId, EntityId)
allocateEntityId (FreshEntityId sz highestId arr indArr) = do
  -- Check if we have exhausted all freed ids
  if sz == highestId
    then do
      let cap = sizeofMutableByteArray arr
          capInd = sizeofMutableByteArray indArr
      -- check if we have space in the id array left, if not just double it
      (arr', indArr') <- if sz >= unsafeShiftR cap 3
        then do
          arr' <- newByteArray (cap * 2)
          copyMutableByteArray arr 0 arr' 0 cap
          indArr' <- newByteArray (capInd * 2)
          copyMutableByteArray indArr 0 indArr' 0 capInd
          pure (arr', indArr')
        else pure (arr, indArr)

      -- add our new id to the end of the array and also set its index mapping
      writeByteArray @Int arr' highestId highestId     -- TODO Use pack here?
      writeByteArray @Word32 indArr' highestId (fromIntegral highestId)
      -- increment the size and the highest ever id
      let st = FreshEntityId (sz + 1) (highestId + 1) arr' indArr'
      -- return the newly generated id
      pure (st, EntityId $ Bitfield highestId)
    else do
      -- reuse the last freed id
      -- For details as to why freed ids are at the end of the array in the first place check
      --  the deallocate method
      reusedId <- Bitfield @Int @Entity <$> readByteArray arr sz
      if get @"generation" reusedId == maxBound -- skip this entity forever. We cannot reliably reuse it anymore
        then allocateEntityId (FreshEntityId (sz + 1) highestId arr indArr)
        else do
          let newEid = pack $ Entity { eid = get @"eid" reusedId, generation = 1 + get @"generation" reusedId, pad = 0, tag = pack $ EntityTag False }
          writeByteArray arr sz (unwrap newEid)
          -- increment the size
          let st = FreshEntityId (sz + 1) highestId arr indArr
          -- return the reused id but increment the generation
          pure (st, EntityId newEid)

deAllocateEntityId :: FreshEntityId -> EntityId -> IO FreshEntityId
deAllocateEntityId old@(FreshEntityId sz _ _ _) _ | sz == 0 = pure old
deAllocateEntityId (FreshEntityId sz highestId arr indArr) (EntityId eid) = do
  -- lookup where in the id array the id to deallocate is
  eidInd <- readByteArray @Word32 indArr (fromIntegral $ get @"eid" eid)

  -- special case: Deallocating the last allocated id simply decreases the size counter
  unless (eidInd == fromIntegral (sz - 1)) $ do
    -- get the last allocated id (at the end of the array by construction. See allocate)
    lastActive <- readByteArray @Int arr (sz - 1)

    -- swap the id to deallocate with the last allocated id
    -- This now means the id we deallocate is at the end of all allocated ids (and before previously freed ids) 
    writeByteArray @Int arr (fromIntegral eidInd) lastActive 
    writeByteArray @Int arr (sz - 1) (unwrap eid)

    -- also swap the places in the index array to keep it up to date
    writeByteArray @Word32 indArr (fromIntegral $ get @"eid" (Bitfield @Int @Entity lastActive)) eidInd 
    writeByteArray @Word32 indArr (fromIntegral $ get @"eid" eid) (fromIntegral $ sz - 1)

  -- decrement the size
  pure $ FreshEntityId (sz - 1) highestId arr indArr

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

data FreshEntityId where
  FreshEntityId ::
       {-# UNPACK #-} !Int -- current number of allocated ids
    -> {-# UNPACK #-} !Int -- highest ever allocated id
    -> {-# UNPACK #-} !(MutableByteArray RealWorld) -- 64 bits. array containing all allocated and freed ids
    -> {-# UNPACK #-} !(MutableByteArray RealWorld) -- 32 bits. maps where in the above array an id is
    -> FreshEntityId

newtype EntityId = EntityId { unEntityId :: Bitfield Int Entity }
  deriving stock Show
  deriving newtype (Eq, Storable)
  deriving HashKey via Int

data Entity = Entity {
  eid        :: {-# UNPACK #-} !Word32
, generation :: {-# UNPACK #-} !Word16
, pad        :: {-# UNPACK #-} !Word8
, tag        :: {-# UNPACK #-} !(Bitfield Word8 EntityTag)
}
  deriving stock (Show, Generic)

data Relation = Relation {
  first  :: {-# UNPACK #-} !Word32
, second :: {-# UNPACK #-} !Word24 -- TODO
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
