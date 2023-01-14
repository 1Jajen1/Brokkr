{-# LANGUAGE RecordWildCards #-}
module Hecs.World.Internal (
  World(..)
, new
, allocateEntityId
, deAllocateEntityId
) where

import Hecs.Entity.Internal (EntityId)
import qualified Hecs.Entity.Internal as EntityId

data World = World {
  freshEId :: {-# UNPACK #-} !EntityId.FreshEntityId
}

new :: IO World
new = do
  freshEId <- EntityId.new
  pure World{..}

allocateEntityId :: World -> IO (World, EntityId)
allocateEntityId w@World{freshEId} = do
  (freshEID', eid) <- EntityId.allocateEntityId freshEId
  pure (w { freshEId = freshEID' }, eid)

deAllocateEntityId :: World -> EntityId -> IO World
deAllocateEntityId w@World{freshEId} eid = do
  freshEID' <- EntityId.deAllocateEntityId freshEId eid
  pure (w { freshEId = freshEID' })
