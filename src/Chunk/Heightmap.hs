{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
module Chunk.Heightmap (
  Heightmaps(..)
, Heightmap(..)
) where

import Control.DeepSeq

import GHC.Generics

import Util.NBT
import Util.Vector.Packed

data Heightmaps = Heightmaps {
  motionBlocking :: Maybe (Heightmap) -- TODO Set defaults
}
  deriving stock (Show, Generic)
  deriving anyclass NFData

instance FromNBT Heightmaps where
  parseNBT = withCompound $ \obj -> do
    motionBlocking <- obj .:? "MOTION_BLOCKING"
    pure Heightmaps{..}
  {-# INLINE parseNBT #-}

instance ToNBT Heightmaps where
  toNBT Heightmaps{..} = case motionBlocking of
    Just hm -> compound ["MOTION_BLOCKING" .= hm]
    Nothing -> compound []

newtype Heightmap = Heightmap (PackedVector ('Static 256) ('Static 9))
  deriving newtype (Show, NFData, FromNBT, ToNBT)
