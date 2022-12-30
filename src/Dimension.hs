module Dimension (
  Dimensions
, new
, Dimension
, getDimension
, withDimensions
, DimensionType(..)
, DimensionName
) where

import qualified Data.Vector as V

import Dimension.Internal

newtype Dimensions = Dimensions (V.Vector Dimension)

new :: IO Dimensions
new = do
  let overworld = Dimension
      nether    = Dimension
      theEnd    = Dimension
  pure . Dimensions $ V.fromListN 3 [overworld, nether, theEnd]

getDimension :: Dimensions -> DimensionType -> Dimension
getDimension (Dimensions arr) Overworld = V.unsafeIndex arr 0
getDimension (Dimensions arr) Nether    = V.unsafeIndex arr 1
getDimension (Dimensions arr) TheEnd    = V.unsafeIndex arr 2

withDimensions :: Dimensions -> (forall f . Foldable f => f Dimension -> a) -> a
withDimensions (Dimensions arr) f = f arr
