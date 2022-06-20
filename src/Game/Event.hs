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
import Control.Monad.State.Strict

data Event =
    PlayerJoined Player
  | PlayerMoved Player (Maybe Position)

-- This changes the gamestate and also sends out changes to network 
applyEvent ::
  ( MonadIO m
  , MonadEntityId m
  , MonadState GameState m
  ) => Connection.Handle -> Event -> m ()
applyEvent conn = \case
  PlayerJoined p -> playerJoined p conn
  PlayerMoved p pos -> playerMoved p pos conn
{-# SPECIALIZE applyEvent :: Connection.Handle -> Event -> StateT GameState (GameM IO) ()  #-}
