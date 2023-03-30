{-# LANGUAGE GADTs #-}
module Network.Packet.Server.Play.PlayerAction (
  PlayerAction(..)
) where

import Block.BlockFace
import Block.Position

import Network.Util

import Util.Binary

data PlayerAction =
    StartDigging  !BlockPosition !BlockFace
  | CancelDigging !BlockPosition !BlockFace
  | FinishDigging !BlockPosition !BlockFace
  | DropItemStack
  | DropItem
  | ShootArrowOrFinishEating
  | SwapItemInHand
  deriving stock (Show, Eq)

instance FromBinary PlayerAction where
  get = get @VarInt >>= \case
    0 -> StartDigging  <$> get <*> get
    1 -> CancelDigging <$> get <*> get
    2 -> FinishDigging <$> get <*> get
    3 -> get @BlockPosition >> get @BlockFace >> pure DropItemStack
    4 -> get @BlockPosition >> get @BlockFace >> pure DropItem
    5 -> get @BlockPosition >> get @BlockFace >> pure ShootArrowOrFinishEating
    6 -> get @BlockPosition >> get @BlockFace >> pure SwapItemInHand
    -- TODO Handle error case
