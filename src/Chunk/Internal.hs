{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
module Chunk.Internal (
  Chunk(..)
, ChunkSection(..)
, ChunkPosition
, pattern ChunkPos
, Heightmaps
) where

import qualified Data.Vector as V
import Chunk.Position
import Chunk.Section
import Util.NBT
import Data.Int
import GHC.Generics (Generic)
import Control.DeepSeq
import Util.Vector.Packed

numSections :: Int
numSections = 26 -- TODO Derive from world height value

data Chunk = Chunk {
  position       :: {-# UNPACK #-} !ChunkPosition
, lowestYSection :: {-# UNPACK #-} !Int
, sections       :: {-# UNPACK #-} !(V.Vector ChunkSection)
, heightmaps     :: {-# UNPACK #-} !Heightmaps
}
  deriving stock Generic
  deriving anyclass NFData

instance FromNBT Chunk where
  parseNBT = withCompound $ \obj -> do
    
    xPos <- fromIntegral @Int32 <$> obj .: "xPos"
    zPos <- fromIntegral @Int32 <$>obj .: "zPos"
    let position = ChunkPos xPos zPos

    lowestYSection <- fromIntegral @Int32 <$>obj .: "yPos"

    sects <- obj .: "sections"
    let sections = V.generate numSections $ \i ->
          let actualI = i - 1 -- Include the -1 section
          in case V.find (\ChunkSection{y} -> y == actualI) sects of
            Just sect -> sect
            Nothing -> emptySection actualI

    heightmaps <- obj .: "Heightmaps"

    pure Chunk{..}

data Heightmaps = Heightmaps {
  motionBlocking :: Heightmap
}
  deriving stock (Show, Generic)
  deriving anyclass NFData

instance FromNBT Heightmaps where
  parseNBT = withCompound $ \obj -> do
    motionBlocking <- obj .: "MOTION_BLOCKING"
    pure Heightmaps{..}

instance ToNBT Heightmaps where
  toNBT Heightmaps{..} = compound ["MOTION_BLOCKING" .= motionBlocking]

newtype Heightmap = Heightmap (PackedVector ('Static 256) ('Static 9))
  deriving newtype (Show, NFData, FromNBT, ToNBT)
