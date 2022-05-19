{-# LANGUAGE TypeFamilies #-}
module Util.Linear.V3 (
  V3(..)
) where

import Control.DeepSeq

-- Use a data family so that GHC unpacks the fields
data family V3 a
data instance V3 Int = V3_Int !Int !Int !Int
  deriving stock (Eq, Show)
data instance V3 Double = V3_Double !Double !Double !Double
  deriving stock (Eq, Show)

instance NFData (V3 Int) where
  rnf (V3_Int _ _ _) = ()
