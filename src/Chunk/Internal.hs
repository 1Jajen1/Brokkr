{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
module Chunk.Internal (
  Chunk(..)
, ChunkSection(..)
, ChunkPosition
, pattern ChunkPos
) where

import qualified Data.Vector as V
import Chunk.Position
import Chunk.Section
import Util.NBT
import Data.Int
import GHC.Generics (Generic)
import Control.DeepSeq

data Chunk = Chunk {
  position       :: {-# UNPACK #-} !ChunkPosition
, lowestYSection :: {-# UNPACK #-} !Int
, sections       :: {-# UNPACK #-} !(V.Vector ChunkSection)
}
  deriving stock Generic
  deriving anyclass NFData

instance FromNBT Chunk where
  parseNBT = withCompound $ \obj -> do
    
    xPos <- fromIntegral @Int32 <$> obj .: "xPos"
    zPos <- fromIntegral @Int32 <$>obj .: "zPos"
    let position = ChunkPos xPos zPos

    lowestYSection <- fromIntegral @Int32 <$>obj .: "yPos"

    sections <- obj .: "sections"

    pure Chunk{..}
