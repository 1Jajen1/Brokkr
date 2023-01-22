{-# LANGUAGE GADTs #-}
module Block.BlockFace (
  BlockFace
) where

import Block.BlockState (Facing(..))

import Data.Word

import Util.Binary

newtype BlockFace = BlockFace (Facing 'True 'True)
  deriving newtype Eq

instance Show BlockFace where
  show (BlockFace Up) = "Top"
  show (BlockFace Down) = "Bottom"
  show (BlockFace x) = show x

instance FromBinary BlockFace where
  get = get @Word8 >>= \case
    0 -> pure $ BlockFace Down
    1 -> pure $ BlockFace Up
    2 -> pure $ BlockFace North
    3 -> pure $ BlockFace South
    4 -> pure $ BlockFace West
    5 -> pure $ BlockFace East
    -- TODO Handle error case

