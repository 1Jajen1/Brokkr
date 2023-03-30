module System.PlayerMovement (
  Translate
, Rotate
, Land, Fly
) where

import Hecs

data Translate
instance Component Translate

data Rotate
instance Component Rotate

data Land
data Fly
