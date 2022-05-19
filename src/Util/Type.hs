{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Util.Type (
  Append
) where

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x:xs) ys = x : Append xs ys
