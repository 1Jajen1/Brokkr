{-# LANGUAGE FunctionalDependencies #-}
module Brokkr.Util.Linear.Vector (
  VectorSpace(..)
, (|-|)
) where

class VectorSpace k v | v -> k where
  (|+|) :: v -> v -> v
  (|*|) :: k -> v -> v

(|-|) :: (Num k, VectorSpace k v) => v -> v -> v
(|-|) l r = l |+| ((-1) |*| r)
