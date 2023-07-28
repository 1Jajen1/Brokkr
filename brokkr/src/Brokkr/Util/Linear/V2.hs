{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Util.Linear.V2 (
  V2(..)
) where

import Brokkr.Util.Linear.Vector

import Control.DeepSeq
import Data.Hashable
import Foreign.Storable
import GHC.Generics
import Hecs

-- Use a data family so that GHC unpacks the fields
data family V2 a
data instance V2 Int = V2_Int !Int !Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass Hashable
  deriving (Component, Storable) via (GenericFlat (V2 Int))

data instance V2 Float = V2_Float !Float !Float
  deriving stock (Eq, Show, Generic)
  deriving (Component, Storable) via (GenericFlat (V2 Float))

instance NFData (V2 Int) where
  rnf (V2_Int _ _) = ()

instance VectorSpace Int (V2 Int) where
  (V2_Int x1 y1) |+| (V2_Int x2 y2) = V2_Int (x1 + x2) (y1 + y2)
  l |*| (V2_Int x y) = V2_Int (l * x) (l * y)

instance VectorSpace Float (V2 Float) where
  (V2_Float x1 y1) |+| (V2_Float x2 y2) = V2_Float (x1 + x2) (y1 + y2)
  l |*| (V2_Float x y) = V2_Float (l * x) (l * y)