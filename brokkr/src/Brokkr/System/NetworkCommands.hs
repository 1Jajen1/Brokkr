module Brokkr.System.NetworkCommands (
  networkCommands
) where

import Brokkr.Packet.ClientToServer.Play qualified as Packet

import Brokkr.Client

import Brokkr.Network.Command
import Brokkr.Network.Connection

import Brokkr.Util.Position
import Brokkr.Util.Rotation

import Brokkr.Server.Monad (Server)
import Brokkr.Server.Monad qualified as Server

import Brokkr.System.PlayerMovement

import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import Data.Coerce
import Data.Foldable

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
-- Movement
handleCommand eid (Packet.SetPlayerPositionAndRotation (Packet.Position posX posY posZ) (Packet.Rotation yaw pitch) onGround)
  = do
    currentPos <- Server.get @Position eid pure $ error "Client without a position" -- TODO errors
    currentRot <- Server.get @Rotation eid pure $ error "Client without a rotation" -- TODO errors
    -- currentFalling <- Server.get @Falling eid pure $ error "Client without falling" -- TODO errors
    Server.set @Position eid (Position posX posY posZ)
    Server.set @Rotation eid (Rotation yaw pitch)
    -- Server.set @Falling  eid onGround
    -- set the old values -- TODO enable the components rather than setting and unsetting every time
    Server.set @OldPosition eid $ coerce currentPos
    Server.set @OldRotation eid $ coerce currentRot
handleCommand eid (Packet.SetPlayerPosition (Packet.Position posX posY posZ) onGround)
  = do
    currentPos <- Server.get @Position eid pure $ error "Client without a position" -- TODO errors
    Server.set @Position eid (Position posX posY posZ)
    -- set the old values
    Server.set @OldPosition eid $ coerce currentPos
    pure ()
handleCommand eid (Packet.SetPlayerRotation (Packet.Rotation yaw pitch) onGround)
  = do
    currentRot <- Server.get @Rotation eid pure $ error "Client without a rotation" -- TODO errors
    Server.set @Rotation eid (Rotation yaw pitch)
    -- set the old values
    Server.set @OldRotation eid $ coerce currentRot
    pure ()
handleCommand eid (Packet.SetPlayerOnGround onGround)
  = do
    pure ()
-- UseItem
-- UseItemOn handles *all* right clicks that target a block. It also triggers with empty slots
-- UseItem is like UseItemOn but only when not targeting a block *and* it won't trigger with an empty slot

-- Ok so how to implement this?
-- If the targeted block has a special action on right click *and* not holding a block while sneaking
--   perform the right click action
-- else
--   if holding a block
--     place that block
--   else
--     do other stuff?
handleCommand eid (Packet.UseItemOn hand pos face cursor inBlock seqId)
  = do
    pure ()
handleCommand eid (Packet.UseItem hand seqId)
  = do
    pure ()

handleCommand eid p = liftIO $ putStrLn $ "Unhandled packet " <> (show p)
