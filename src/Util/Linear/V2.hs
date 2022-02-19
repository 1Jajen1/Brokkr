{-# LANGUAGE TypeFamilies #-}
module Util.Linear.V2 (
  V2(..)
) where

-- Use a data family so that GHC unpacks the fields
data family V2 a
data instance V2 Int = V2_Int !Int !Int
