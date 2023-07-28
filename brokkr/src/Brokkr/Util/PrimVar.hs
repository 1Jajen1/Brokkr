module Brokkr.Util.PrimVar (
  PrimVar
, newPrimVar
, readPrimVar
, writePrimVar
) where

import Data.Primitive
import Control.Monad.Primitive

newtype PrimVar s a = PrimVar (MutableByteArray s)

newPrimVar :: forall a m . (PrimMonad m, Prim a) => a -> m (PrimVar (PrimState m) a)
newPrimVar el = PrimVar <$> do
  mar <- newByteArray (sizeOf el)
  writeByteArray mar 0 el
  pure mar
{-# INLINE newPrimVar #-}

readPrimVar :: (PrimMonad m, Prim a) => PrimVar (PrimState m) a -> m a
readPrimVar (PrimVar arr) = readByteArray arr 0
{-# INLINE readPrimVar #-}

writePrimVar :: (PrimMonad m, Prim a) => PrimVar (PrimState m) a -> a -> m ()
writePrimVar (PrimVar arr) = writeByteArray arr 0
{-# INLINE writePrimVar #-}
