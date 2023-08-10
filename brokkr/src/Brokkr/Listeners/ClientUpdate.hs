module Brokkr.Listeners.ClientUpdate (
  addClient
, removeClient
) where

import Brokkr.Client
import Brokkr.Client.Username

import Brokkr.Network.Connection

import Brokkr.Server.Monad (EntityId, Server)
import Brokkr.Server.Monad qualified as Server

import Brokkr.Packet.Encode qualified as Encode
import Brokkr.Packet.ServerToClient.Play qualified as SC.Play
import Brokkr.Packet.Common.Internal qualified as SC.Play.Unsafe

import Control.Monad.IO.Class (liftIO)

import Hecs.Array qualified as Arr
import Data.Coerce (coerce)
import Data.Vector qualified as V

-- Gets called when an entity receives the 'Client' tag. At this point it is expected to already have a Connection, a Username and a ClientUUID
--
-- Iterates all existing connections and sends them the new player. Then sends the new player all existing connections
-- TODO Maybe instead of a Client Tag, add a Tag for the protocol state?
--  => Make this change when the configuration phase gets added
addClient :: EntityId -> Server ()
addClient newPlayerEid = do
  newConn <- Server.get @Connection newPlayerEid pure $ error "New client without a connection" -- TODO Errors
  newName <- Server.get @Username   newPlayerEid pure $ error "New client without a name"
  newUUID <- Server.get @ClientUUID newPlayerEid pure $ error "New client without a uuid"

  let newUpdate = (coerce newUUID, updateNew $ coerce newName)

  -- send new connection to all existing connections and accumulate all current players for later
  -- TODO getting all players should instead read from a global how many players are on, then pre-allocate a mutable vector and use that instead of Hecs.Array
  arr <- liftIO $ Arr.new 8 >>= \arr -> Arr.writeBack arr newUpdate

  acc <- Server.filter (Server.filterDSL @'[Server.Tag Client, Connection, Username, ClientUUID]) (\aty acc0 -> do
    connRef     <- Server.getColumn @Connection aty
    usernameRef <- Server.getColumn @Username   aty
    uuidRef     <- Server.getColumn @ClientUUID aty
    Server.iterateArchetype aty (\n _ acc -> do
      conn  <- Server.readColumn connRef     n
      uname <- Server.readColumn usernameRef n
      uuid  <- Server.readColumn uuidRef     n
      liftIO . sendPacket @UnsafeDefDimHeight conn
             . Encode.Packet (Encode.EstimateMin 32) -- TODO Estimate correctly
             $ SC.Play.PlayerInfoUpdate (SC.Play.PlayerInfoUpdates $ V.singleton newUpdate)
      liftIO $ Arr.writeBack acc (coerce uuid, updateNew . SC.Play.Unsafe.UnsafeUsername $ coerce uname) -- TODO This is indeed unsafe as I have no checks on Username
      ) (pure acc0)
    ) (pure arr)

  vec <- liftIO $ V.generateM (Arr.size acc) $ Arr.read acc

  liftIO $ sendPacket @UnsafeDefDimHeight newConn
    . Encode.Packet (Encode.EstimateMin 32) -- TODO Estimate correctly
    $ SC.Play.PlayerInfoUpdate (SC.Play.PlayerInfoUpdates vec)
  where
    updateNew uname =
      SC.Play.InfoAddPlayer uname `SC.Play.combinePlayerInfo`
      SC.Play.InfoListed True

removeClient :: EntityId -> Server ()
removeClient removedPlayerEid = do
  removedUUID <- Server.get @ClientUUID removedPlayerEid pure $ error "Removed client without a uuid"

  Server.filter (Server.filterDSL @'[Server.Tag Client, Connection]) (\aty _ -> do
    connRef <- Server.getColumn @Connection aty
    Server.iterateArchetype_ aty $ \n _ -> do
      conn <- Server.readColumn connRef n
      liftIO . sendPacket @UnsafeDefDimHeight conn
             . Encode.Packet (Encode.EstimateMin 32) -- TODO Estimate correctly
             $ SC.Play.PlayerInfoRemove (SC.Play.PlayerInfoRemoves $ V.singleton $ coerce removedUUID)
    ) (pure ())
