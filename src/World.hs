module World (
  Dimension(..)
, World
, HasDimension(..)
, getOrLoadChunk
, getOrLoadChunks
, unloadChunk
, chunk
) where

import World.Internal

import qualified Util.Queue as Queue
import qualified IO.ChunkCache as CK
import qualified IO.Chunkloading as Chunkloading
import Chunk.Internal
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Optics
import IO.ChunkCache (ChunkCache)
import Command.World

-- methods
-- TODO These really should not be in Internal!
getOrLoadChunk :: World -> ChunkPosition -> (Chunk -> IO ()) -> IO World
getOrLoadChunk w cp act
  -- TODO Should I fork act c here?
  | Just c <- w ^? chunk cp = act c >> pure w'
  | otherwise = do
    Chunkloading.loadChunk (chunkloading w) cp $ \case
      Just !c -> do
        -- Doing this before act ensures the next tick will have the chunks
        -- If we were to do this after act we could have a client already modifying the chunks
        -- but the chunks aren't in the cache. The window for that is really narrow but
        -- it does exist
        Queue.push (_commandQueue w) $! CacheLoadedChunk c 
        act c
      Nothing -> error "Worldgen not implemented"
    pure w'
  where w' = w { _chunkCache = CK.loadChunk (_chunkCache w) cp }
{-# INLINE getOrLoadChunk #-}

getOrLoadChunks :: World -> V.Vector ChunkPosition -> (Chunk -> IO ()) -> IO World
getOrLoadChunks w cps act = do
  let (res, toLoad) = V.partitionWith (\cp ->
        case w ^? chunk cp of
          -- TODO fork act c here?
          Just c -> Left $ act c
          Nothing -> Right cp
        ) cps
  sequence_ res >> Chunkloading.loadChunks (chunkloading w) toLoad (act' . fromMaybe (error "Worldgen not implemented"))
  -- TODO This isn't too pretty
  pure $ w { _chunkCache = V.foldl' (\acc cp -> CK.loadChunk acc cp) (_chunkCache w) cps }
  where
    act' !c = do
      Queue.push (_commandQueue w) $! CacheLoadedChunk c
      act c
{-# INLINE getOrLoadChunks #-}

-- This does not directly unload the chunk, just removes one loading source from it and unloads it if that happens to be the final one
unloadChunk :: World -> ChunkPosition -> World
unloadChunk w@World{_chunkCache} cp = w { _chunkCache = CK.unloadChunk _chunkCache cp }
{-# INLINE unloadChunk #-}

-- lenses
cLens :: Lens' World ChunkCache
cLens = lens _chunkCache $ \w nC -> w { _chunkCache = nC }
{-# INLINE cLens #-}

-- TODO Check if loaded first
chunk :: ChunkPosition -> AffineTraversal' World Chunk
chunk cp = cLens %  CK.chunk cp
{-# INLINE chunk #-}
