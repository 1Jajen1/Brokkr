{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Network.Packet.Server.Play.PlayerAbilities (
  Abilities(..)
) where

import GHC.Generics

data Abilities = Abilities {
  _pad :: Bool
, flying :: Bool
}
  deriving stock (Show, Generic)
