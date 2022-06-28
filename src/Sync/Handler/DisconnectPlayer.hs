module Sync.Handler.DisconnectPlayer (
  disconnectPlayer
) where

import Player
import Sync.Monad
import Event
import Util.Disconnect (DisconnectReason)

disconnectPlayer :: Player -> DisconnectReason -> Sync [Event]
disconnectPlayer p r = pure [PlayerDisconnected p r]
