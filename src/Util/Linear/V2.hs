{-# LANGUAGE TypeFamilies #-}
module Util.Linear.V2 (
  V2(..)
) where

import Control.DeepSeq

-- Use a data family so that GHC unpacks the fields
data family V2 a
data instance V2 Int = V2_Int !Int !Int
  deriving stock (Eq)

instance Show (V2 Int) where
  show (V2_Int a b) = show a <> " " <> show b

instance NFData (V2 Int) where
  rnf (V2_Int _ _) = ()
