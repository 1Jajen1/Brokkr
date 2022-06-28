module Command.Handler.World (
  handle
) where

import Command.World
import World
import Sync.Monad
import Event
import Optics

handle :: Command -> World -> Sync [Event]
handle (CacheLoadedChunk c) w = pure [ChunkCached (w ^. dimension) c]
