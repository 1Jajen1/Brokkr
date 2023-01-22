module Dimension (
  Dimension
, DimensionType(..)
, RegionFilePath
, DimensionName
) where

import Hecs

data Dimension
instance Component Dimension

data DimensionType = Overworld | Nether | TheEnd

data RegionFilePath
instance Component RegionFilePath

data DimensionName
instance Component DimensionName
