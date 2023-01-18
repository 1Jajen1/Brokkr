module Dimension (
  Dimension
, Overworld
, Nether
, TheEnd
, DimensionName
, RegionFilePath
) where

import Hecs

data Dimension
instance Component Dimension

data Overworld
instance Component Overworld

data Nether
instance Component Nether

data TheEnd
instance Component TheEnd

data DimensionName
instance Component DimensionName

data RegionFilePath
instance Component RegionFilePath
