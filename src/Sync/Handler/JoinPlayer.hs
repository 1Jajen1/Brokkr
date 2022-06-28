module Sync.Handler.JoinPlayer (
  joinPlayer
) where

import Player
import Game.State
import Sync.Monad
import Event

joinPlayer :: Player -> GameState -> Sync [Event]
-- TODO Check for bans and other conditions here
joinPlayer p _ = pure [PlayerJoined p, PlayerMoved p Nothing]
