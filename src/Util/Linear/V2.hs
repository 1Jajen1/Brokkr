{-# LANGUAGE TypeFamilies #-}
module Util.Linear.V2 (
  V2(..)
) where

import Control.DeepSeq

-- Use a data family so that GHC unpacks the fields
data family V2 a
data instance V2 Int = V2_Int !Int !Int
  deriving stock (Eq, Show)
data instance V2 Float = V2_Float !Float !Float
  deriving stock (Eq, Show)

instance NFData (V2 Int) where
  rnf (V2_Int _ _) = ()
