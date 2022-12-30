module Dimension.Internal (
  Dimension(..)
, DimensionType(..)
, DimensionName(..)
) where

import Data.String (IsString)
import Data.Text (Text)

import Network.Util.MCString (MCString(..))

import Util.Binary

data Dimension = Dimension {
  -- chunkCache
  -- entities
  -- dimension settings
  -- dimension name and type
}

data DimensionType = Overworld | Nether | TheEnd
  deriving stock (Show, Eq)

newtype DimensionName = DimensionName Text
  deriving stock Show
  deriving newtype (Eq, IsString)
  deriving (ToBinary, FromBinary) via MCString
