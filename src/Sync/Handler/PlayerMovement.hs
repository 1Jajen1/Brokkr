module Sync.Handler.PlayerMovement (
  movePlayer
, rotatePlayer
, updateMovingPlayer
) where

import Player
import Util.Position
import Game.State
import Sync.Monad
import Event
import Util.Rotation
import Optics

movePlayer :: Player -> Position -> GameState -> Sync [Event]
movePlayer p pos _ = pure [PlayerMoved p (Just pos)]

 -- Rotate and onGround updates only matter for the associated entitiy right?
rotatePlayer :: Player -> Rotation -> GameState -> Sync [Event]
rotatePlayer _p _rot _ = pure []

updateMovingPlayer :: Player -> OnGround -> GameState -> Sync [Event]
updateMovingPlayer p newOnGround _st
  | p ^. onGround /= newOnGround =
    -- Check if we need to apply fall-damage here
    pure [] -- TODO
  | otherwise = pure []
