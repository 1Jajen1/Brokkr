{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Monad (
  ServerM
, Server
, newWorld
, runServerM
, getUniverse
, Universe
, getSingleton
, getComponentId, filterDSL, getColumn
, module Hecs
, system
) where

import Hecs hiding (getComponentId, filterDSL, getColumn)
import qualified Hecs
import Hecs.Monad

import Control.Concurrent.MVar

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Trans.Control

import Data.Coerce
import Data.Proxy

-- Components
-- Network
import {-# SOURCE #-} Client (Joined)
import {-# SOURCE #-} Network.Connection (Connection)
-- Common
import Util.Position (Position, Falling)
import Util.Rotation (Rotation)
import Util.UUID (UUID)
import Util.Velocity (Velocity)
-- Dimension
import {-# SOURCE #-} Dimension (Dimension, Overworld, Nether, TheEnd, DimensionName, RegionFilePath)
-- misc
import IO.Chunkloading (Chunkloading)

makeWorld "Universe"
  [ -- Network related components
    ''Joined -- Tag which is added once a player fully joined the server. Every (globally known) connection without this is either not a player or has not joined
  , ''Connection -- Self contained connection, has everything the network thread needs to work. Can also disconnect the client
    -- Common components
  , ''Position -- Position and velocity both have a V3 Double underneath. This could be modeled with (Position, V3 Double) and (Velocity, V3 Double) instead
  , ''Velocity --  but this encoding makes it easier to have type safe functions where this distinction is necessary
  , ''Rotation -- Same as above, this is technically V2 Float, but it makes sense to use the newtype
  , ''Falling -- TODO I might want this to be a tag instead. Might have to change a little bit of logic on the network side then
  -- Dimension stuff
  , ''Dimension
  , ''Overworld
  , ''Nether
  , ''TheEnd
  , ''DimensionName
  , ''RegionFilePath
  -- misc
  , ''Chunkloading
  ]

type Server a = ServerM IO a

newtype ServerM m a = ServerM (HecsM Universe m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadBase b, MonadBaseControl b)

deriving newtype instance (MonadBase IO m, MonadBaseControl IO m) => MonadHecs Universe (ServerM m)

runServerM :: MonadBaseControl IO m => Universe -> ServerM m a -> m a
runServerM universe (ServerM h) = runHecsM universe $ do
  h

getUniverse :: MonadIO m => ServerM m Universe
getUniverse = ServerM getWorld

getSingleton :: forall c m . (NoTagBackend (Backend c) ReadTagMsg, Has Universe c, MonadHecs Universe m) => m c
getSingleton =
  Hecs.getComponent @c (coerce $ Hecs.getComponentId @Universe @c)
    pure
    (error "Singleton missing")
{-# INLINE getSingleton #-}

getComponentId :: forall c . Has Universe c => ComponentId c
getComponentId = Hecs.getComponentId @Universe @c
{-# INLINE getComponentId #-}

filterDSL :: forall xs . FilterDSL Universe (FilterFromList xs) => Filter (FilterFromList xs) (HasMain (FilterFromList xs))
filterDSL = Hecs.filterDSL @Universe @xs
{-# INLINE filterDSL #-}

getColumn :: forall c ty m . (Has Universe c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> m (Backend c)
getColumn = Hecs.getColumn @Universe @c
{-# INLINE getColumn #-}

system :: Filter ty HasMainId -> (TypedArchetype ty -> Server ()) -> Server ()
system fi act = Hecs.defer $ do
  Hecs.filter fi
    (\aty _ -> act aty)
    (pure ())
{-# INLINE system #-}
