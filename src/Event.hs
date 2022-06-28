module Event (
  Event(..)
) where

import Chunk
import Player
import Util.Position
import Util.Disconnect
import World

data Event =
    PlayerJoined !Player
  | PlayerMoved !Player !(Maybe Position)
  | PlayerDisconnected !Player !DisconnectReason
  | ChunkCached !Dimension !Chunk
  deriving stock Show
