{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module World.Internal (
  WorldName(..)
, Handle(worldName, dimension, chunkManager)
, new
, Dimension(..)
) where

import Data.Text
import Network.Util.MCString
import Util.Binary
import Data.String
import qualified Effect.ChunkManager as ChunkManager

data Handle = Handle {
    worldName           :: WorldName
  , dimension           :: Dimension
  , chunkManager        :: ChunkManager.Handle
  }

new :: WorldName -> Dimension -> ChunkManager.Handle -> Handle
new worldName dimension chunkManager = Handle{..}

newtype WorldName = WorldName Text
  deriving stock Show
  deriving newtype (Eq, IsString)
  deriving (ToBinary, FromBinary) via MCString

data Dimension = Overworld | Nether | TheEnd

