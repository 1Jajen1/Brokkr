module Block.GlobalPalette (
  globalPalette
) where

import Data.Int
import qualified Data.Vector.Unboxed as U

import Block.BlockState
import Block.Internal.Conversion
import GHC.TypeLits

globalPalette :: U.Vector Int16
globalPalette = undefined--U.generate maxVal $ \i -> 1
  where maxVal = fromIntegral $ natVal @HighestBlockStateId undefined
