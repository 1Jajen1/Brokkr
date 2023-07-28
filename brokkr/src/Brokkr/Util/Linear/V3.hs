{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Util.Linear.V3 (
  V3(..)
) where

import Brokkr.Util.Linear.Vector

import Control.DeepSeq
import Foreign.Storable
import GHC.Generics
import Hecs

-- Use a data family so that GHC unpacks the fields
data family V3 a
data instance V3 Int = V3_Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Show, Generic)
  deriving (Component, Storable) via (GenericFlat (V3 Int))
data instance V3 Float = V3_Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  deriving stock (Eq, Show, Generic)
  deriving (Component, Storable) via (GenericFlat (V3 Float))
data instance V3 Double = V3_Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving stock (Eq, Show, Generic)
  deriving (Component, Storable) via (GenericFlat (V3 Double))

instance NFData (V3 Int) where
  rnf (V3_Int _ _ _) = ()
instance NFData (V3 Float) where
  rnf (V3_Float _ _ _) = ()
instance NFData (V3 Double) where
  rnf (V3_Double _ _ _) = ()

instance VectorSpace Int (V3 Int) where
  (V3_Int x1 y1 z1) |+| (V3_Int x2 y2 z2) = V3_Int (x1 + x2) (y1 + y2) (z1 + z2)
  l |*| (V3_Int x y z) = V3_Int (l * x) (l * y) (l * z)

instance VectorSpace Float (V3 Float) where
  (V3_Float x1 y1 z1) |+| (V3_Float x2 y2 z2) = V3_Float (x1 + x2) (y1 + y2) (z1 + z2)
  l |*| (V3_Float x y z) = V3_Float (l * x) (l * y) (l * z)

instance VectorSpace Double (V3 Double) where
  (V3_Double x1 y1 z1) |+| (V3_Double x2 y2 z2) = V3_Double (x1 + x2) (y1 + y2) (z1 + z2)
  l |*| (V3_Double x y z) = V3_Double (l * x) (l * y) (l * z)
