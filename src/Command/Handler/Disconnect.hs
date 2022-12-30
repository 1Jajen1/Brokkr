module Command.Handler.Disconnect (
  handle
) where

import Command.Handler

import Control.Concurrent.STM
import Control.Monad.STM.Class

import qualified Entity.EntityId as EntityId

import qualified Network.Connection as Conn

import Client (Client)
import qualified Client

import qualified Server

import Util.UUID

handle :: Client -> Handler ()
handle p = do
  server <- getServer
  liftSTM $ writeTVar (Client.joined p) False
  -- TODO This is just bare minimum cleanup.
  unsafeTellAction $ do
    -- TODO
    EntityId.deAllocateEntityId (Server.freshEntityId server) (Client.entityId p)
    Client.removeClient (Server.connectedClients server) (uuid p)
    Conn.close (Client.connection p)
