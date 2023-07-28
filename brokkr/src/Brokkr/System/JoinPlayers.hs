module Brokkr.System.JoinPlayers (
  joinPlayers
) where

import Brokkr.Client
import Brokkr.Network.Connection

import Brokkr.Server.Monad as Server

-- iterate all connections with Not Joined
-- TODO Change to query, although this is probably never going to matter perf wise
joinPlayers :: Server ()
joinPlayers = Server.system
  (Server.filterDSL @'[Connection, Server.Not (Server.Tag Joined)])
  $ \aty -> do
    connections <- Server.getColumn @Connection aty
    Server.iterateArchetype aty $ \n eid -> do
      _conn <- Server.readColumn connections n

      -- The player is technically on the server by now, but other players don't know yet

      -- TODO Actually join instead of just setting the tag...
      Server.addTag @Joined eid
