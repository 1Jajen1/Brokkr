module Event.Handler (
  apply
) where
import Control.Monad.IO.Class
import Entity.Id.Monad
import Control.Monad.State.Strict
import Game.State
import Event
import Game.Monad
import Event.Handler.PlayerJoined (playerJoined)
import Event.Handler.PlayerMoved (playerMoved)
import Event.Handler.PlayerDisconnected (playerDisconnected)
import Event.Handler.ChunkCached (chunkCached)

-- TODO Move. This is an implementation detail and should not be specific to Event directly, so move out of this file
-- This changes the gamestate and also sends out changes to network 
apply ::
  ( MonadIO m
  , MonadEntityId m
  , MonadState GameState m
  ) => Event -> m ()
apply (PlayerJoined p) = playerJoined p

apply (PlayerMoved p pos) = playerMoved p pos

apply (PlayerDisconnected p reason) = playerDisconnected p reason

apply (ChunkCached dim c) = chunkCached dim c
{-# SPECIALIZE apply :: Event -> StateT GameState (GameM IO) ()  #-}

