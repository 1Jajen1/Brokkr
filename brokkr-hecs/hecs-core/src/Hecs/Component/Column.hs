{-# LANGUAGE TypeFamilies #-}
module Hecs.Component.Column (
  Column(..)
, readColumn, writeColumn
) where

import Control.Monad.Primitive

import Data.Coerce
import Data.Primitive

import Foreign.Storable

import Hecs.Component.Internal

data family Column (kind :: ComponentKind) s a

newtype instance Column Boxed s a = BoxedColumn (MutableArray s a)
newtype instance Column Flat  s a = FlatColumn (MutableByteArray s)
data    instance Column Tag   s a

readColumn :: forall c m r . (Component c, PrimMonad m, Coercible c (ComponentValueFor c)) => Column (ComponentKindFor c) (PrimState m) c -> Int -> (c -> m r) -> m r
{-# INLINE readColumn #-}
readColumn = backing @_ @c
  (\(BoxedColumn arr) n cont -> readArray arr n >>= cont)
  (\(FlatColumn  arr) n cont -> let p = mutableByteArrayContents arr in do
    el <- coerce <$> unsafeIOToPrim (peekElemOff @(ComponentValueFor c) (coerce p) n)
    cont el
    )
  (error "Impossible. A value of type Storage Tag does not exist")

writeColumn :: forall c m . (Component c, PrimMonad m, Coercible c (ComponentValueFor c)) => Column (ComponentKindFor c) (PrimState m) c -> Int -> c -> m ()
{-# INLINE writeColumn #-}
writeColumn = backing @_ @c
  (\(BoxedColumn arr) n el -> writeArray arr n el)
  (\(FlatColumn  arr) n el -> let p = mutableByteArrayContents arr in coerce <$> unsafeIOToPrim (pokeElemOff @(ComponentValueFor c) (coerce p) n (coerce el)))
  (error "Impossible. A value of type Storage Tag does not exist")

