{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Orphan specilization
module Server (
  ServerM
, Server
, newWorld
, runServerM
, getUniverse
, Universe
, getSingleton
, getComponentId
, getColumn
, filterDSL
, component
, system
) where

import Hecs
import Hecs.Monad (getWorld)

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Trans.Control

import Data.Coerce

-- Components
-- Network
import Client (Joined)
import {-# SOURCE #-} Network.Connection (Connection)
-- Common
import Util.Position (Position, Falling)
import Util.Rotation (Rotation)
-- import Util.UUID (UUID)
import Util.Velocity (Velocity)
import Util.Hecs (Previous)
-- Dimension
import {-# SOURCE #-} Dimension (Dimension, DimensionType(..), DimensionName, RegionFilePath)
-- misc
import IO.Chunkloading (Chunkloading)
-- Systems
import {-# SOURCE #-} System.PlayerMovement (Translate, Rotate, Land, Fly)

makeWorld "Universe"
  [ -- Network related components
    ''Joined -- Tag which is added once a player fully joined the server. Every (globally known) connection without this is either not a player or has not joined
  , ''Connection -- Self contained connection, has everything the network thread needs to work. Can also disconnect the client
    -- Common components
  , ''Position -- Position and velocity both have a V3 Double underneath. This could be modeled with (Position, V3 Double) and (Velocity, V3 Double) instead
  , ''Velocity --  but this encoding makes it easier to have type safe functions where this distinction is necessary
  , ''Rotation -- Same as above, this is technically V2 Float, but it makes sense to use the newtype
  , ''Falling
  -- Common utils
  , ''Previous
  -- Dimension stuff
  , ''Dimension
  , 'Overworld, 'Nether, 'TheEnd
  , ''DimensionName
  , ''RegionFilePath
  -- misc
  , ''Chunkloading
  -- Systems
  , ''Translate, ''Rotate, ''Land, ''Fly
  ]

type Server a = ServerM IO a

newtype ServerM m a = ServerM (HecsM Universe m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadBase b, MonadBaseControl b)

deriving newtype instance (MonadBase IO m, MonadBaseControl IO m) => MonadHecs Universe (ServerM m)

runServerM :: Universe -> ServerM m a -> m a
runServerM universe (ServerM h) = runHecsM universe h

getUniverse :: MonadIO m => ServerM m Universe
getUniverse = ServerM getWorld

getSingleton :: forall c m . (BranchRel c, Component c, Has Universe c, MonadHecs Universe m) => m c
getSingleton =
  get @c (coerce $ getComponentId @c)
    pure
    (error "Singleton missing")
{-# INLINE getSingleton #-}

system :: Filter ty HasMainId -> (TypedArchetype ty -> Server ()) -> Server ()
system fi act = defer $ do
  Hecs.filter fi
    (\aty _ -> act aty)
    (pure ())
{-# INLINE system #-}
