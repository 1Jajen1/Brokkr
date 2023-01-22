module System.JoinPlayers (
  joinPlayers
) where

import Client
import Network.Connection

import Server as Hecs
import Hecs

-- iterate all connections with Not Joined
-- TODO Change to query, although this is probably never going to matter perf wise
joinPlayers :: Server ()
joinPlayers = Hecs.system
  (Hecs.filterDSL @'[Connection, Hecs.Not (Hecs.Tag Joined)])
  $ \aty -> do
    connections <- Hecs.getColumn @Connection aty
    Hecs.iterateArchetype aty $ \n eid -> do
      _conn <- Hecs.readColumn connections n

      -- The player is technically on the server by now, but other players don't know yet

      -- TODO Actually join instead of just setting the tag...
      Hecs.addTag @Joined eid
