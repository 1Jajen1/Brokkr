{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Hecs.Filter.DSL (
  filterDSL
, FilterDSL(..)
, ToFilter
, HasMain
) where

import Data.Kind
import Data.Type.Bool
import Data.Proxy

import GHC.TypeLits

import Hecs.Component.Internal
import Hecs.Component.Relation
import Hecs.Filter.Internal
import Hecs.World.Has

filterDSL :: forall w fi . FilterDSL w fi => Filter (HasMain fi) (ToFilter fi)
filterDSL = toFilter @w @fi
{-# INLINE filterDSL #-}

type family HasMain xs :: Bool where
  HasMain '[]           = False
  HasMain  (And l r:xs) = HasMain l || HasMain r || HasMain xs
  HasMain  (x:xs)       = True

type family ToFilter xs where
  ToFilter '[]     = TypeError (Text "Empty filters are not supported")
  ToFilter '[c]    = c
  ToFilter  (x:xs) = And x (ToFilter xs)

class FilterDSL (w :: Type) xs where
  toFilter :: Filter (HasMain xs) (ToFilter xs)

instance (Component c, Has w c, FilterDSL w (x:xs), HasMain (c:x:xs) ~ True, ToFilter (c:x:xs) ~ And c (ToFilter (x:xs))) => FilterDSL w (c:x:xs) where
  toFilter = filterAnd
    (componentWithId (getComponentId @_ @_ @c (Proxy @w)))
    (toFilter @w @(x:xs))
  {-# INLINE toFilter #-}

instance (Component c, Has w c, HasMain '[c] ~ True, ToFilter '[c] ~ c) => FilterDSL w '[c] where
  toFilter = componentWithId (getComponentId @_ @_ @c (Proxy @w))
  {-# INLINE toFilter #-}

