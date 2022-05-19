{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}
module Effect.World (
  Worlds(..)
, WorldManager
, runWorldManager
, withWorld
) where

import Effectful
import World.Internal (Dimension(..))
import qualified World.Internal as World
import Effectful.Dispatch.Static

data Worlds = Worlds !World.Handle !World.Handle !World.Handle

type role WorldManager phantom phantom
data WorldManager :: Effect
type instance DispatchOf WorldManager = 'Static
newtype instance StaticRep WorldManager = WorldState Worlds

runWorldManager :: Worlds -> Eff (WorldManager : es) a -> Eff es a
runWorldManager w = evalStaticRep (WorldState w)

withWorld :: WorldManager :> es => Dimension -> (World.Handle -> Eff es a) -> Eff es a
withWorld dim act = do
  WorldState (Worlds overworld nether end) <- getStaticRep @WorldManager
  case dim of
    Overworld -> act overworld
    Nether    -> act nether
    TheEnd    -> act end
