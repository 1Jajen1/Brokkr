module Brokkr.Dimension (
  Dimension
, DimensionType(..)
, DimensionName
) where

import Hecs

data Dimension
instance Component Dimension

data DimensionType = Overworld | Nether | TheEnd

data DimensionName
instance Component DimensionName
