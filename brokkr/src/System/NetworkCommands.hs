module System.NetworkCommands (
  networkCommands
) where

import Client
import Control.Monad.Trans.Control

import Data.Coerce
import Data.Foldable

import Network.Connection

import Util.Linear.Vector
import Util.Position
import Util.Rotation

import Server.Monad (Server)
import Server.Monad qualified as Server

import System.PlayerMovement

networkCommands :: Server ()
networkCommands = Server.system
  (Server.filterDSL @'[Server.Tag Joined, Connection])
  $ \aty -> do
    connRef <- Server.getColumn @Connection aty
    Server.iterateArchetype aty $ \n eid -> do
      conn <- Server.readColumn connRef n
      st <- liftBaseWith $ \runInBase -> flushCommands conn $ runInBase . traverse_ (handleCommand eid)
      restoreM st

handleCommand :: Server.EntityId -> Command -> Server ()

handleCommand eid (MoveAndRotateClient pos rot onGround)
  = do
    currentPos <- Server.get @Position eid pure $ error "Client without a position" -- TODO errors
    currentRot <- Server.get @Rotation eid pure $ error "Client without a rotation" -- TODO errors
    Server.set eid . Translate . coerce $ pos |-| currentPos
    Server.set eid . Rotate    . coerce $ rot |-| currentRot
handleCommand eid (MoveClient pos onGround)
  = do
    currentPos <- Server.get @Position eid pure $ error "Client without a position" -- TODO errors
    Server.set eid . Translate . coerce $ pos |-| currentPos
handleCommand eid (RotateClient rot onGround)
  = do
    currentRot <- Server.get @Rotation eid pure $ error "Client without a rotation" -- TODO errors
    Server.set eid . Rotate    . coerce $ rot |-| currentRot
handleCommand eid (SetOnGround onGround)
  = do
    pure ()
