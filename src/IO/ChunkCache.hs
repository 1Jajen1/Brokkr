{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IO.ChunkCache (
  ChunkCache
, emptyChunkCache
, chunk
, loadChunk
, unloadChunk
, insertChunk
) where

import Chunk

import Data.IntMap.Strict ( IntMap )

import Optics
import Data.Bits
import qualified Data.IntMap.Strict as IM
import Data.Tuple (swap)

-- For each chunk we store a reference counter and the chunk
-- It is possible for a chunk to be loaded but not yet in the cache
data ChunkCache = ChunkCache !(IntMap Int) !(IntMap Chunk)
  
instance Show ChunkCache where
  show (ChunkCache loaded _) = show $ fmap (swap . fmap fromInd . swap) $ IM.toList loaded

emptyChunkCache :: ChunkCache
emptyChunkCache = ChunkCache mempty mempty

chunk :: ChunkPosition -> AffineTraversal' ChunkCache Chunk
chunk cp =
  lens (\(ChunkCache _ chunks) -> chunks) (\(ChunkCache l _) c -> ChunkCache l c) % at (toIndex cp) % _Just
{-# INLINE chunk #-}

loadChunk :: ChunkCache -> ChunkPosition -> ChunkCache
loadChunk (ChunkCache loaded chunks) cp = ChunkCache loaded' chunks
  where
    ind = toIndex cp
    loaded' = IM.alter (maybe (Just 1) (Just . (+ 1))) ind loaded
{-# INLINE loadChunk #-}

unloadChunk :: ChunkCache -> ChunkPosition -> ChunkCache
unloadChunk (ChunkCache loaded chunks) cp = ChunkCache loaded' chunks'
  where
    ind = toIndex cp
    loaded' = IM.alter (maybe Nothing $ \i -> if i == 1 then Nothing else Just $ i - 1) ind loaded
    chunks' = if IM.member ind loaded'
      then chunks
      else IM.delete ind chunks
{-# INLINE unloadChunk #-}

insertChunk :: ChunkCache -> Chunk -> ChunkCache
insertChunk ck@(ChunkCache loaded chunks) c
  | IM.member ind loaded = ChunkCache loaded $ IM.insert ind c chunks
  | otherwise = ck
  where ind = toIndex $ c ^. chunkPosition
{-# INLINE insertChunk #-}

--
toIndex :: ChunkPosition -> Int
toIndex (ChunkPos x z) = (mask .&. x) .|. (z `unsafeShiftL` 32)
  where mask = (1 `unsafeShiftL` 32) - 1
{-# INLINE toIndex #-}

fromInd :: Int -> ChunkPosition
fromInd i = ChunkPos x z
  where
    mask = (1 `unsafeShiftL` 32) - 1
    x = if testBit i 31
      then (i .&. mask) .|. (mask `unsafeShiftL` 32)
      else i .&. mask
    z = i `unsafeShiftR` 32
{-# INLINE fromInd #-}
