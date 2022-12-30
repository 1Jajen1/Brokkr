module Command.Handler.Movement (
  handleMove
, handleTurn
, handleOnGround
) where

import Command.Handler

import Control.Monad.STM.Class

import Client

import Util.Position
import Util.Rotation

handleMove :: Client -> Position -> Handler ()
handleMove c p = liftSTM $ setPosition c p 

handleTurn :: Client -> Rotation -> Handler ()
handleTurn c r = liftSTM $ setRotation c r

handleOnGround :: Client -> OnGround -> Handler ()
handleOnGround p _ = pure () -- liftSTM . writeTVar (onGround p)
