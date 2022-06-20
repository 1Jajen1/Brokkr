{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Game.Event.PlayerMoved (
  playerMoved
) where

import Game.State
import Util.Position
import Player
import qualified Data.HashSet as HS
import Chunk.Position
import qualified Network.Packet.Client.Play as C
import qualified Data.Vector as V
import Network.Connection
import qualified Network.Connection as Connection
import Optics
import Util.UUID
import Network.Packet.Client.Play.ChunkData
import Control.Monad.IO.Class
import World.Internal
import Game.Monad (GameM)
import Util.Rotation
import qualified Data.HashMap.Strict as HM
import Chunk.Internal
import Control.Monad.State.Strict

{- TODO: This needs a rewrite

The good:
- Extremely simple, all player moves will go through here

The bad:
- Chunkloading is slow, creating the chunk data packet is slow
- At least now multiple player movement events are not merged

Going forward Chunkloading and sending should be offloaded from the sync gametick

Wins:
- We can prefetch chunks from anywhere (even when the player just joins)
- ticks will be a lot faster when chunkloading happens

Needs:
- A way to append the new chunks to the gamestate at the start of the new gametick
  => Scheduling tasks to sync should just execute them first, easy enough
- Some guarantee that we won't not yet loaded chunks
  => This is more difficult, but just having a set of loaded chunks and doing a
     membership test before sending Unload should work. This does however mean we
     have to be able to cancel sending
- Some way to prevent duplicate chunk loads
  => We need to store in-flight requests anyway so that's that...
  => Fuck this if something duplicates then so be it

- When are chunks actually loaded by a player? When they are sent or when they are queued? 
  => Presumable the former, but that complicates things...

Random thoughts:
- Implement a loaded chunks checks only based on position and viewDistance
- Chunk loading/sending and unloading:
  - Send new chunks at the end of the tick and check if they are in the players
    loading area, also append the chunk to the players loaded chunks if so
      - Doing this at the end means we won't waste time sending newly unloaded chunks
      - We also add the chunks to the worlds loaded chunks here
      - This is a bulk operation, but not necessarily a complete one, ie more such operations may happen in the next ticks
  - Unloading a chunk first checks if a chunk is actually loaded by the player
- Chunks in the world should be stored together with a reference count
  - when adding to the players loaded chunk list this count is increased, when removed it is decreased
  - it is possible for the count to be 0 (temporarily) but the next decrement will evict it
  - We may want to keep some chunks around for a little longer, but thats for future me to worry about
  - Perma loaded chunks like spawnchunks or forceloaded will simply always have a ref count of 1
  - Temporary chunks have count 0 and are added to the unloaded set directly after use
    - Maybe this should just bypass the chunk cache, little benefit keeping them
  - getOrLoadChunk will increase the ref count by 1 even if it wasn't loaded, so
    use `chunk cp` if in sync, unless you actually intend to load the chunk 

Sooo:
loadChunk:
  - queue a bunch of chunkloads, have n queues indexed by the regionfile they are currently holding and a free list of queues not assigned
  - This returns IO Chunk, the respective IO blocks and returns only when the Chunk is actually loaded
  - Then queue a sync action to append this chunk to the players loaded chunks

appendToPlayer action:
  - check if the chunk is still required by the player
  - if so append this chunk to the players loaded chunk list
  - if not perform a check if we still need this chunk and evict if not
    - eviction could mean writing to an unloaded set and purging the oldest
      members of that set once a threshold is crossed

end every gametick:
  - quick check if we have loaded *any* chunks
  - if so we add those the the world they belong to at the end of the tick

-}
playerMoved ::
  ( MonadIO m
  , MonadState GameState m
  ) => Player -> Maybe Position -> Connection.Handle -> m ()
playerMoved p mpos conn = do
  st <- get
  let !w = st ^. world dim
  !cs <- case mpos of
    Nothing -> liftIO (updateViewPosition w conn p Nothing)
    Just pos -> do
      let !p' = position .~ pos $ p
          !oldPos = p ^. position
          !st' = (player uid % _Just) .~ p' $ st
      put st' >> liftIO (updateViewPosition w conn p' (Just oldPos))
  
  st <- get
  let !w' = w { _chunks = _chunks w <> cs }
  put $! world dim .~ w' $ st
  where
    !uid = p ^. uuid
    !dim = p ^. dimension
{-# SPECIALIZE playerMoved :: Player -> Maybe Position -> Connection.Handle -> StateT GameState (GameM IO) () #-}

-- https://stackoverflow.com/questions/398299/looping-in-a-spiral for a fast inclusivity check, tho having bitmaps seems fine as well
-- since we don't need one per player as we can shift them
updateViewPosition :: World -> Connection.Handle -> Player -> Maybe Position -> IO (HM.HashMap ChunkPosition Chunk)
updateViewPosition !w !conn !p !mPrevPos = do
  let !newChunkPos = p ^. position % chunkPosition
      !(ChunkPos x z) = newChunkPos
      !newChunks = HS.fromList [ChunkPos x1 z1 | x1 <- [(x - viewDistance) .. (x + viewDistance)], z1 <- [(z - viewDistance) .. (z + viewDistance)]]
      !viewDistance = 8
  case mPrevPos of
    Just prevPos -> do
      let oldChunkPos = prevPos ^. chunkPosition
          ChunkPos oldX oldZ = oldChunkPos
          oldChunks = HS.fromList [ChunkPos x1 z1 | x1 <- [(oldX - viewDistance) .. (oldX + viewDistance)], z1 <- [(oldZ - viewDistance) .. (oldZ + viewDistance)]]
          toSend = HS.difference newChunks oldChunks
          toUnload = HS.difference oldChunks newChunks
      if (oldChunkPos /= newChunkPos)
        then do
          sendPacket conn (10, C.UpdateViewPosition x z)
          sendPackets conn $ (\(ChunkPos x' z') -> (16, C.UnloadChunk x' z')) <$> V.fromList (HS.toList toUnload)
          sendChunks toSend
        else pure mempty
    Nothing -> do
      sendPacket conn (10, C.UpdateViewPosition x z)
      -- TODO Handle teleport id correctly
      sendPacket conn (40, C.PlayerPositionAndLook (p ^. position) (p ^. rotation) (C.TeleportId 0) C.NoDismount)
      sendChunks newChunks
  where
    sendChunks :: HS.HashSet ChunkPosition -> IO (HM.HashMap ChunkPosition Chunk)
    sendChunks toSend = do
      !(ps, cs) <- foldMapHS getChunkAndChunkDataPacket toSend
      sendPackets conn $ V.fromList ps
      pure cs
    {-# INLINE sendChunks #-}
    foldMapHS f = HM.foldMapWithKey (\a _ -> f a) . HS.toMap
    {-# INLINE foldMapHS #-}
    -- Todo this runs in sync, either move async or make fast enough!
    -- Ideas: Prefetch chunks async for first join, but load sync otherwise?
    getChunkAndChunkDataPacket :: ChunkPosition -> IO ([(Int, C.PlayPacket)], HM.HashMap ChunkPosition Chunk)
    getChunkAndChunkDataPacket cPos = getOrLoadChunk w cPos >>= \c ->
        let !(!cSize, !cData) = mkChunkData c
        in pure ([(cSize, C.ChunkDataAndUpdateLight cData)], HM.singleton cPos c)
    {-# INLINE getChunkAndChunkDataPacket #-}

{-

data Word128 = W128#  W64#  W64#
data Word256 = W256# W128# W128#
...

instance Bits Word128 where
  shiftL (W128# a b) = W128# (shiftL a 1 .|. shiftR b 63) (shiftL b 1)

data Width = W8 | W16 | W32 | ... 

data ChunkBitSet = ChunkBitSet !Width !ByteArray

moveVert, MoveHor :: ChunkBitSet -> Int -> ChunkBitSet
moveVert, MoveHor = case width of
  W8 -> moveUp' @Word8
  ...

Move up/down: drop first/last row, append/prepend empty row (one in-place copy + memset)
Move left/right: shift each row left/right by one

union/intersection = bitwise or/and over all rows
difference a b = bitwise and a (not b)

iterate ẞẞ

-}
