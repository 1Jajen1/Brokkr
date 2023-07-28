module Brokkr.Client.GameMode (
  GameMode(..)
) where

data GameMode = Survival | Creative | Adventure | Spectator 
  deriving stock (Show, Eq)
