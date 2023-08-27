{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Orphan specilization
module Brokkr.Server.Monad (
  ServerM
, Server
, newWorld
, runServerM
, getConfig
, getUniverse
, Universe
, getSingleton
, getComponentId
, getColumn
, getColumnM
, filterDSL
, component
, system
, module Hecs
, module Brokkr.Server.Config
) where

import Brokkr.Server.Config

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader

import Control.Monad.Primitive

import Data.Coerce

import Hecs
import Hecs.Monad (getWorld)

import Brokkr.Debug.Monad

-- Components
-- Network
import Brokkr.Client
import Brokkr.Client.Username
import Brokkr.Network.Connection (Connection)
-- Common
import Brokkr.Util.Position (Position, Falling)
import Brokkr.Util.Rotation (Rotation)
import Brokkr.Inventory.Internal (SomeInventory)
-- import Util.UUID (UUID)
import Brokkr.Util.Velocity (Velocity)
-- Dimension
import {-# SOURCE #-} Brokkr.Dimension (Dimension, DimensionType(..), DimensionName)
import Brokkr.IO.ChunkCache (SomeChunkCache)
-- Systems
import {-# SOURCE #-} Brokkr.System.JoinPlayers
import {-# SOURCE #-} Brokkr.System.PlayerMovement

makeWorld "Universe"
  [ -- Network related components
    ''Connection -- Self contained connection, has everything the network thread needs to work. Can also disconnect the client
    -- Common components
  , ''Position -- Position and velocity both have a V3 Double underneath. This could be modeled with (Position, V3 Double) and (Velocity, V3 Double) instead
  , ''Velocity --  but this encoding makes it easier to have type safe functions where this distinction is necessary
  , ''Rotation -- Same as above, this is technically V2 Float, but it makes sense to use the newtype
  , ''Falling
  , ''SomeInventory
  -- Common utils
  -- Dimension stuff
  , ''Dimension
  , 'Overworld, 'Nether, 'TheEnd
  , ''DimensionName
  , ''SomeChunkCache
  -- Systems
  , ''OldPosition, ''OldRotation, ''ChunkYPosition
  -- OnGround state
  , ''Land, ''Fly
  -- other player move states
  , ''Sneaking, ''Sprinting, ''Flying
  -- Join player signal tag
  , ''JoinPlayer, ''RemovePlayer
  -- client identifiers
  , ''Client, ''Username, ''ClientUUID
  ]

type Server a = ServerM IO a

newtype ServerM m a = ServerM (TraceT Verbose (ReaderT Config (HecsM Universe m)) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b, MonadTrace, PrimMonad)

instance MonadTrans ServerM where
  lift = ServerM . lift . lift . lift

deriving newtype instance (MonadBase IO m, MonadBaseControl IO m) => MonadHecs Universe (ServerM m)

runServerM :: Config -> Universe -> ServerM m a -> m a
runServerM conf universe (ServerM h) = runHecsM universe $ runReaderT (runTraceTStdOut h) conf

getConfig :: Monad m => ServerM m Config
getConfig = ServerM $ lift ask

getUniverse :: MonadIO m => ServerM m Universe
getUniverse = ServerM . lift $ lift getWorld

getSingleton :: forall c m . (BranchRel c, Component c, Has Universe c, MonadHecs Universe m) => m c
getSingleton =
  get @c (coerce $ getComponentId @c)
    pure
    (error "Singleton missing")
{-# INLINE getSingleton #-}

system :: Filter ty HasMainId -> (TypedArchetype ty -> Server ()) -> Server ()
system fi act = defer $ do
  Hecs.runFilter fi
    (\aty _ -> act aty)
    (pure ())
{-# INLINE system #-}
