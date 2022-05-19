{-# LANGUAGE PatternSynonyms #-}
module Util.Position (
  Position(..)
, pattern Position
) where

import Util.Linear.V3
import Util.Binary

newtype Position = Pos (V3 Double)
  deriving stock Show

pattern Position :: Double -> Double -> Double -> Position
pattern Position x y z = Pos (V3_Double x y z)
{-# COMPLETE Position #-}

-- TODO Do I need/want relative positioning?
instance ToBinary Position where
  put (Position x y z) = put x <> put y <> put z 
