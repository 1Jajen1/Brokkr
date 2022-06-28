{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module World.Internal (
  WorldName(..)
, World(..)
, Dimension(..)
, HasDimension(..)
, getOrLoadChunk
, getOrLoadChunks
, unloadChunk
, chunk
) where

import Data.Text
import Network.Util.MCString
import Util.Binary
import Data.String
import Optics
import qualified IO.Chunkloading as Chunkloading
import Chunk.Internal
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import IO.ChunkCache ( ChunkCache )
import qualified IO.ChunkCache as CK
import Command.World
import Util.Queue (Queue)
import World.Dimension
import qualified Util.Queue as Queue

data World = World {
  _worldName     :: !WorldName
, _dimension     :: !Dimension
, chunkloading   :: {-# UNPACK #-} !Chunkloading.Handle
, _chunkCache    :: {-# UNPACK #-} !ChunkCache
, _commandQueue  :: Queue Command
}
  deriving stock Show

newtype WorldName = WorldName Text
  deriving stock Show
  deriving newtype (Eq, IsString)
  deriving (ToBinary, FromBinary) via MCString

-- methods
-- TODO Add commands to world and add CacheChunk command here
getOrLoadChunk :: World -> ChunkPosition -> (Chunk -> IO ()) -> IO World
getOrLoadChunk w cp act
  -- TODO Should I fork act c here?
  | Just c <- w ^? chunk cp = act c >> pure w'
  | otherwise = do
    Chunkloading.loadChunk (chunkloading w) cp $ \case
      -- TODO Prepend queueing adding to the chunk cache here
      Just c -> do
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
  -- TODO Prepend queueing adding to the chunk cache to act
  sequence_ res >> Chunkloading.loadChunks (chunkloading w) toLoad (act' . fromMaybe (error "Worldgen not implemented"))
  -- TODO This isn't too pretty
  pure $ w { _chunkCache = V.foldl' (\acc cp -> CK.loadChunk acc cp) (_chunkCache w) cps }
  where
    act' c = do
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

instance HasDimension World where
  type Access World = A_Getter
  dimension = to _dimension
  {-# INLINE dimension #-}
