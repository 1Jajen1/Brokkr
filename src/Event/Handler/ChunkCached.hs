module Event.Handler.ChunkCached (
  chunkCached
) where

import Control.Monad.State.Strict
import Game.State
import World.Internal
import Chunk
import Optics
import qualified IO.ChunkCache as CK

chunkCached :: MonadState GameState m => Dimension -> Chunk -> m ()
chunkCached dim c = modify' $ world dim %~ \w@World{_chunkCache} -> w { _chunkCache = CK.insertChunk _chunkCache c }
{-# INLINE chunkCached #-}
