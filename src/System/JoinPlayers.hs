module System.JoinPlayers (
  joinPlayers
) where

import Control.Monad.IO.Class

import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Network.Packet.Client.Play as C
import qualified Network.Packet.Client.Play.Login as C 

import Registry.Biome
import Registry.Dimension

import Client
import Client.GameMode
import Network.Connection

import Block.Position

import Util.Position
import Util.Rotation

import Monad (Server)
import qualified Monad as Hecs

-- iterate all connections with Not Joined
-- TODO Change api to use something like FoldM from foldl or similar to avoid all of this boilerplate
-- TODO Change to query, although this is probably never going to matter perf wise
joinPlayers :: Server ()
joinPlayers = Hecs.system
  (Hecs.filterDSL @'[Connection, Hecs.Not Joined])
  $ \aty -> do
    connections <- Hecs.getColumn @Connection aty
    Hecs.iterateArchetype aty $ \n eid -> do
      conn <- Hecs.readStored connections n

      -- By this point we have chunks already, so we need to send Login (Play)

      -- TODO Actually join instead of just setting the tag...
      Hecs.setTag @Joined eid
