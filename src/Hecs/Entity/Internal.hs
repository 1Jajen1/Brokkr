module Hecs.Entity.Internal (
  EntityId(..)
, FreshEntityId(..)
, new
, allocateEntityId
, deAllocateEntityId
) where

import Control.Monad
import Control.Monad.Primitive

import Data.Int

import Data.Primitive.ByteArray

new :: IO FreshEntityId
new = do
  -- Allocate id storage with space for 256 entity ids...
  arr <- newByteArray 1024
  indArr <- newByteArray 1024
  pure $ FreshEntityId 0 0 arr indArr

allocateEntityId :: FreshEntityId -> IO (FreshEntityId, EntityId)
allocateEntityId (FreshEntityId sz highestId arr indArr) = do
  -- Check if we have exhausted all freed ids
  if sz == highestId
    then do
      let cap = sizeofMutableByteArray arr
      -- check if we have space in the id array left, if not just double it
      -- This may seem like a lot of memory, but 256 entities are just as much
      -- storage as minimum a single chunk section needs for just blockstates, so this is nothing
      -- even if doubled a few times
      (arr', indArr') <- if sz == cap
        then do
          arr' <- newByteArray (cap * 2)
          copyMutableByteArray arr' 0 arr 0 cap
          indArr' <- newByteArray (cap * 2)
          copyMutableByteArray indArr' 0 indArr 0 cap
          pure (arr', indArr')
        else pure (arr, indArr)
      -- add our new id to the end of the array and also set its index mapping
      writeByteArray @Int32 arr' highestId (fromIntegral highestId)
      writeByteArray @Int32 indArr' highestId (fromIntegral highestId)
      -- increment the size and the highest ever id
      let st = FreshEntityId (sz + 1) (highestId + 1) arr' indArr'
      -- return the newly generated id
      pure $ (st, EntityId highestId)
    else do
      -- reuse the last freed id
      -- For details as to why freed ids are at the end of the array in the first place check
      --  the deallocate method
      newId <- fromIntegral <$> readByteArray @Int32 arr sz
      -- increment the size
      let st = FreshEntityId (sz + 1) highestId arr indArr
      -- return the reused id
      pure $ (st, EntityId newId)

deAllocateEntityId :: FreshEntityId -> EntityId -> IO FreshEntityId
deAllocateEntityId (old@(FreshEntityId sz highestId arr indArr)) (EntityId eid) = do
  if sz == 0
    -- if we have no allocated ids, just stop
    -- TODO: Maybe this, and trying to deallocate an unallocated id, should throw exceptions
    --       Trying to do that will eventually lead to weird errors if the id got reused in the meantime
    then pure old
    else do
      -- lookup where in the id array the id to deallocate is
      eidInd <- readByteArray @Int32 indArr eid

      -- special case: Deallocating the last allocated id simply decreases the size counter
      unless (fromIntegral eidInd == sz - 1) $ do
        -- get the last allocated id (at the end of the array by construction. See allocate)
        lastActive <- readByteArray @Int32 arr (sz - 1)

        -- swap the id to deallocate with the last allocated id
        -- This now means the id we deallocate is at the end of all allocated ids (and before previously freed ids) 
        writeByteArray @Int32 arr (fromIntegral eidInd) lastActive 
        writeByteArray @Int32 arr (sz - 1) (fromIntegral eid)

        -- also swap the places in the index array to keep it up to date
        writeByteArray @Int32 indArr (fromIntegral lastActive) eidInd 
        writeByteArray @Int32 indArr eid (fromIntegral $ sz - 1)

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

data FreshEntityId = FreshEntityId
  {-# UNPACK #-} !Int -- current number of allocated ids
  {-# UNPACK #-} !Int -- highest ever allocated id
  {-# UNPACK #-} !(MutableByteArray RealWorld) -- array containing all allocated and freed ids
  {-# UNPACK #-} !(MutableByteArray RealWorld) -- maps where in the above array an id is

newtype EntityId = EntityId { unEntityId :: Int }
  deriving stock Show
  deriving newtype Eq
