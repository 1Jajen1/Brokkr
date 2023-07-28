{-# LANGUAGE RoleAnnotations #-}
module Hecs.Filter.Internal (Tag) where

type role Tag phantom
data Tag (x :: k)
