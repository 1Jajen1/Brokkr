module Game.Event (
  Event(..)
, applyEvent
) where

import Game.State
import Control.Monad.IO.Class
import Player
import Game.Event.PlayerJoined ( playerJoined )
import Util.Position
import Game.Event.PlayerMoved (playerMoved)
import qualified Network.Connection as Connection
import Entity.Id.Monad
import Game.Monad (GameM)
import Control.Monad.Trans.State.Strict (StateT)

data Event =
    PlayerJoined Player
  | PlayerMoved Player (Maybe Position)

-- This changes the gamestate and also sends out changes to network 
applyEvent ::
  ( MonadIO m
  , MonadEntityId m
  ) => GameState -> Connection.Handle -> Event -> m GameState
applyEvent st conn = \case
  PlayerJoined p -> playerJoined p st conn
  PlayerMoved p pos -> playerMoved p pos st conn
{-# SPECIALIZE applyEvent :: GameState -> Connection.Handle -> Event -> StateT GameState (GameM IO) GameState  #-}
