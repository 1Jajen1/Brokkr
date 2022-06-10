{-# LANGUAGE FunctionalDependencies #-}
module Util.Linear.Vector (
  VectorSpace(..)
) where

class VectorSpace k v | v -> k where
  (|+|) :: v -> v -> v
  (|*|) :: k -> v -> v
