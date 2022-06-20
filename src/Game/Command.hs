module Game.Command (
  Command(..)
) where

import Player.Internal
import Util.Position
import Util.Rotation

-- At some point this will get an additional param for what resources the command requires
-- Then the whole thing will be a GADT where each command requires different resources
data Command =
    JoinPlayer Player
  | MovePlayer Position
  | RotatePlayer Rotation
  | UpdateMovingPlayer OnGround
  deriving stock (Eq, Show)
