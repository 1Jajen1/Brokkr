module Brokkr.System.JoinPlayers (
  joinPlayers, removePlayers
, JoinPlayer, RemovePlayer
) where

import Brokkr.Client.Username

import Brokkr.Network.Connection

import Brokkr.Server.Monad (Server)
import Brokkr.Server.Monad qualified as Server

import Brokkr.Packet.Encode qualified as Encode
import Brokkr.Packet.Common.Internal qualified as SC.Play.Unsafe
import Brokkr.Packet.ServerToClient.Play qualified as SC.Play

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Data.Coerce (coerce)
import Data.Vector qualified as V

import Hecs.Array qualified as Arr

data JoinPlayer
data RemovePlayer

-- Reactive system. New players get Join tag which invokes this system
joinPlayers :: Server ()
joinPlayers = Server.defer $ do
  allNew <- Arr.new 8

  -- TODO queries here. Also queries should store their sz and short circuit fast
  Server.runFilter_ (Server.filterDSL @'[Server.Tag JoinPlayer, Connection, Username, ClientUUID])
    $ Server.cmapM_ $ \(newName :: Username, newUUID :: ClientUUID) -> Arr.writeBack allNew (coerce newUUID, updateNew $ coerce newName)

  newSz <- Arr.size allNew
  newVec <- V.generateM newSz $ Arr.read allNew

  when (newSz > 0) $ do
    Server.runFilter_ (Server.filterDSL @'[Server.Not (Server.Tag JoinPlayer), Connection, Username, ClientUUID])
      $ Server.cmapM_ $ \(conn, name :: Username, uuid :: ClientUUID) -> do
        liftIO $ sendPacket @UnsafeDefDimHeight conn
          . Encode.Packet (Encode.EstimateMin 32) -- TODO
          $ SC.Play.PlayerInfoUpdate (SC.Play.PlayerInfoUpdates newVec)
        Arr.writeBack allNew (coerce uuid, updateNew $ coerce name)

    fullSz <- Arr.size allNew 
    fullVec <- V.generateM fullSz $ Arr.read allNew

    Server.runFilter_ (Server.filterDSL @'[Server.Tag JoinPlayer, Connection, Username, ClientUUID])
      $ Server.cmapM_ $ \(eid, newConn) -> do
        liftIO $ sendPacket @UnsafeDefDimHeight newConn
          . Encode.Packet (Encode.EstimateMin 32) -- TODO
          $ SC.Play.PlayerInfoUpdate (SC.Play.PlayerInfoUpdates fullVec)
        Server.removeTag @JoinPlayer eid

  where
    updateNew uname =
      SC.Play.InfoAddPlayer uname `SC.Play.combinePlayerInfo`
      SC.Play.InfoListed True

removePlayers :: Server ()
removePlayers = Server.defer $ do
  allRem <- Arr.new 8

  -- TODO queries here
  Server.runFilter_ (Server.filterDSL @'[Server.Tag RemovePlayer, ClientUUID])
    $ Server.cmapM_ $ \(eid, newUUID :: ClientUUID) -> do
      Arr.writeBack allRem (coerce newUUID)
      Server.freeEntity eid

  sz <- Arr.size allRem 
  vec <- V.generateM sz $ Arr.read allRem

  when (sz > 0) $
    Server.runFilter_ (Server.filterDSL @'[Server.Not (Server.Tag JoinPlayer), Server.Not (Server.Tag RemovePlayer), Connection])
      $ Server.cmapM_ $ \conn -> do
        liftIO . sendPacket @UnsafeDefDimHeight conn
          . Encode.Packet (Encode.EstimateMin 32) -- TODO
          $ SC.Play.PlayerInfoRemove (SC.Play.PlayerInfoRemoves vec)
