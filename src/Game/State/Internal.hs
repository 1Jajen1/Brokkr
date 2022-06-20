{-# LANGUAGE RecordWildCards  #-}
module Game.State.Internal (
  GameState(..)
) where
  
import qualified Data.HashMap.Strict as HM
import World
import Data.UUID
import qualified Network.Connection as Connection
import Player

data GameState = GameState {
  _worlds      :: {-# UNPACk #-} !(World, World, World)
, _players     :: !(HM.HashMap UUID Player)
, _connections :: !(HM.HashMap UUID Connection.Handle)
}
  deriving stock Show
