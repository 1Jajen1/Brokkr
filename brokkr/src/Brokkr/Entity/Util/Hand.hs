module Brokkr.Entity.Util.Hand (
  Hand(..)
) where

data Hand = Main | Off
  deriving stock (Show, Eq)
