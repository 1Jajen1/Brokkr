{-# LANGUAGE RecordWildCards #-}
module Server (
  Server(..)
, new
) where

import qualified Client

import Dimension (Dimensions)
import qualified Dimension

import Entity.EntityId (FreshEntityId)
import qualified Entity.EntityId as EntityId

data Server = Server {
  freshEntityId    :: FreshEntityId
, connectedClients :: Client.Clients
, dimensions       :: Dimensions
}

new :: IO Server
new = do
  connectedClients <- Client.new
  freshEntityId <- EntityId.new
  dimensions <- Dimension.new
  pure Server{..}
