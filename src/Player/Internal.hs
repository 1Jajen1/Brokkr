{-# LANGUAGE TypeFamilies #-}
module Player.Internal (
  Player(..)
, OnGround(..)
, onGround
) where

import Util.Binary
import Util.Position
import Util.Rotation
import Data.UUID
import Player.GameMode
import Util.UUID
import Optics
import World

{- Note: [Players and Entities]

Players are not themselves entities, but they are associated with one.
The assoc entity may or may not be a player entitiy. This has two usages:
- Players can become any entity, so disguise plugins are trivially supported
- Player entities can exist without actual players

-}

-- TODO Remove the players rotation and onGround status, we only need position here for chunkloading, move
-- the rest to the players entitity
-- Also remove position and replace by chunkPosition?
data Player = Player {
    _gameMode   :: GameMode
  , _position   :: Position
  , _rotation   :: Rotation
  , _onGround   :: OnGround
  , _dimension   :: Dimension
  , _uuid       :: UUID
}
  deriving stock Show

instance Eq Player where
  Player{_uuid=u1} == Player{_uuid=u2} = u1 == u2

data OnGround = OnGround | InAir
  deriving stock (Eq, Show)

instance FromBinary OnGround where
  get = get @Bool >>= \case
    True -> pure OnGround
    False -> pure InAir

-- lenses
onGround :: Lens' Player OnGround
onGround = lens _onGround $ \p nG -> p { _onGround = nG }
{-# INLINE onGround #-}

instance HasUUID Player where
  uuid = to _uuid
  {-# INLINE uuid #-}

instance HasPosition Player where
  type Access Player = A_Lens
  position = lens _position $ \p nP -> p { _position = nP }
  {-# INLINE position #-}

instance HasRotation Player where
  type Access Player = A_Lens
  rotation = lens _rotation $ \p nR -> p { _rotation = nR }
  {-# INLINE rotation #-}

instance HasDimension Player where
  type Access Player = A_Lens
  dimension = lens _dimension $ \p nD -> p { _dimension = nD }
  {-# INLINE dimension #-}
