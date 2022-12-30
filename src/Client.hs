{-# LANGUAGE RecordWildCards #-}
module Client (
  entityId, connection, joined
, Client
, GameMode(..)
, Clients
, new
, withClients
, addClient
, removeClient
, getPosition, setPosition
, getRotation, setRotation
, update
) where

import Client.Internal hiding (new)
import Client.GameMode

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.IORef

import Util.UUID

data Clients = Clients {
  ref :: IORef (HashMap UUID Client)
}

new :: IO Clients
new = do
  ref <- newIORef mempty
  pure Clients{..}

withClients :: Clients -> (forall f . Foldable f => f Client -> IO a) -> IO a
withClients Clients{ref} f = do
  clients <- readIORef ref
  f clients
{-# INLINE withClients #-}

addClient :: Clients -> Client -> IO ()
addClient Clients{ref} c = atomicModifyIORef' ref $ \hm -> (HM.insert (uuid c) c hm, ())

removeClient :: Clients -> UUID -> IO ()
removeClient Clients{ref} uid = atomicModifyIORef' ref $ \hm -> (HM.delete uid hm, ())
