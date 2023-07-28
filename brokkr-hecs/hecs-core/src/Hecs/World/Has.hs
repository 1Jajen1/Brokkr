module Hecs.World.Has (
  Has(..)
) where
import Data.Kind
import Data.Proxy
import Hecs.Component.Internal

-- A mapping from World -> ComponentId. A ComponentId from one World is not valid in another
class Has (w :: Type) (c :: k) where
  getComponentId :: Proxy w -> ComponentId c
