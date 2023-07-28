module Brokkr.System.PlayerMovement (
  OldPosition
, OldRotation
, Land, Fly
) where

import Hecs

data OldPosition
instance Component OldPosition

data OldRotation
instance Component OldRotation

data Land
data Fly
