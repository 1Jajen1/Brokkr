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

import Server (Server)
import qualified Server as Hecs
import qualified Hecs

import System.PlayerMovement

networkCommands :: Server ()
networkCommands = Hecs.system
  (Hecs.filterDSL @'[Hecs.Tag Joined, Connection])
  $ \aty -> do
    connRef <- Hecs.getColumn @Connection aty
    Hecs.iterateArchetype aty $ \n eid -> do
      conn <- Hecs.readColumn connRef n
      st <- liftBaseWith $ \runInBase -> flushCommands conn $ runInBase . traverse_ (handleCommand eid)
      restoreM st

handleCommand :: Hecs.EntityId -> Command -> Server ()

handleCommand eid (MoveAndRotateClient pos rot onGround)
  = do
    currentPos <- Hecs.get @Position eid pure $ error "Client without a position" -- TODO errors
    currentRot <- Hecs.get @Rotation eid pure $ error "Client without a rotation" -- TODO errors
    Hecs.set eid . Translate . coerce $ pos |-| currentPos
    Hecs.set eid . Rotate    . coerce $ rot |-| currentRot
handleCommand eid (MoveClient pos onGround)
  = do
    currentPos <- Hecs.get @Position eid pure $ error "Client without a position" -- TODO errors
    Hecs.set eid . Translate . coerce $ pos |-| currentPos
handleCommand eid (RotateClient rot onGround)
  = do
    currentRot <- Hecs.get @Rotation eid pure $ error "Client without a rotation" -- TODO errors
    Hecs.set eid . Rotate    . coerce $ rot |-| currentRot
handleCommand eid (SetOnGround onGround)
  = do
    pure ()
