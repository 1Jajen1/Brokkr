{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Hecs.Component.Generic (
  GenericFlat(..)
) where

import Foreign.Storable
import GHC.Generics
import Data.Kind
import Foreign.Ptr
import Data.Coerce
import Control.Monad

import Hecs.Component.Internal

-- TODO Benchmark, look here for how to:
-- https://hackage.haskell.org/package/derive-storable-plugin
-- https://github.com/mkloczko/derive-storable-benchmark/tree/plugin

-- TODO Only allows product types atm. Should be enough for most ecs uses anyway
newtype GenericFlat a = GenericFlat a

instance (Generic a, GSizeOf (Rep a), GAlignment (Rep a), GPoke (Rep a), GPeek (Rep a)) => Component (GenericFlat a) where
  type ComponentKind (GenericFlat a) = Flat
  type Value (GenericFlat a) = GenericFlat a
  backing _ _ f = f
  {-# INLINE backing #-}

instance (Generic a, GSizeOf (Rep a), GAlignment (Rep a), GPoke (Rep a), GPeek (Rep a)) => Storable (GenericFlat a) where
  sizeOf _ = sz + pad
    where
      pad = mod sz (gAlignment (undefined @_ @(Rep a _)))
      sz = gSizeOf (undefined @_ @(Rep a _))
  {-# INLINE sizeOf #-}
  alignment _ = gAlignment (undefined @_ @(Rep a _))
  {-# INLINE alignment #-}
  poke ptr (GenericFlat a) = void $ gPoke 0 (coerce ptr) (from a)
  {-# INLINE poke #-}
  peek = fmap (GenericFlat . to . snd) . gPeek 0 . coerce
  {-# INLINE peek #-}

class GSizeOf (f :: Type -> Type) where
  gSizeOf :: f x -> Int

instance GSizeOf f => GSizeOf (M1 i m f) where
  gSizeOf _ = gSizeOf (undefined @_ @(f _))
  {-# INLINE gSizeOf #-}

instance (GAlignmentNext g, GSizeOf f, GSizeOf g) => GSizeOf (f :*: g) where
  gSizeOf _ =
    let leftSz = gSizeOf (undefined @_ @(f _))
        nextAlign = gAlignmentNext (undefined @_ @(g _))
        pad = mod leftSz nextAlign
    in leftSz + pad + gSizeOf (undefined @_ @(g _))
  {-# INLINE gSizeOf #-}

instance Storable a => GSizeOf (K1 i a) where
  gSizeOf _ = sizeOf (undefined @_ @a)
  {-# INLINE gSizeOf #-}

class GAlignment (f :: Type -> Type) where
  gAlignment :: f x -> Int

instance GAlignment f => GAlignment (M1 i m f) where
  gAlignment _ = gAlignment (undefined @_ @(f _))
  {-# INLINE gAlignment #-}

instance (GAlignment f, GAlignment g) => GAlignment (f :*: g) where
  gAlignment _ = max (gAlignment (undefined @_ @(f _))) (gAlignment (undefined @_ @(g _)))
  {-# INLINE gAlignment #-}

instance Storable a => GAlignment (K1 i a) where
  gAlignment _ = alignment (undefined @_ @a)
  {-# INLINE gAlignment #-}

class GPoke (f :: Type -> Type)  where
  gPoke :: Int -> Ptr () -> f x -> IO Int

instance GPoke f => GPoke (M1 i m f) where
  gPoke w ptr (M1 f) = gPoke w ptr f
  {-# INLINE gPoke #-}

instance (GAlignmentNext g, GPoke f, GPoke g) => GPoke (f :*: g) where
  gPoke w ptr (f :*: g) = do
    w' <- gPoke w ptr f
    let pad = mod w' nextAlign
        w'' = pad + w'
    gPoke w'' ptr g
    where
      nextAlign = gAlignmentNext (undefined @_ @(g _))
  {-# INLINE gPoke #-}

instance Storable a => GPoke (K1 m a) where
  gPoke w ptr (K1 a) = poke (coerce $ ptr `plusPtr` w) a >> pure (w + sizeOf (undefined @_ @a))
  {-# INLINE gPoke #-}

class GPeek (f :: Type -> Type) where
  gPeek :: Int -> Ptr () -> IO (Int, f x)

instance GPeek f => GPeek (M1 i m f) where
  gPeek w = fmap (fmap M1) . gPeek w
  {-# INLINE gPeek #-}

instance (GAlignmentNext g, GPeek f, GPeek g) => GPeek (f :*: g) where
  gPeek w ptr = do
    (w', f) <- gPeek w ptr
    let pad = mod w' nextAlign
        w'' = pad + w'
    fmap (f :*:) <$> gPeek w'' ptr
    where
      nextAlign = gAlignmentNext (undefined @_ @(g _))
  {-# INLINE gPeek #-}

instance Storable a => GPeek (K1 m a) where
  gPeek w = fmap (\a -> (w + sizeOf (undefined @_ @a), K1 a)) . peek . coerce . (`plusPtr` w)
  {-# INLINE gPeek #-}

class GAlignmentNext (f :: Type -> Type) where
  gAlignmentNext :: f x -> Int

instance GAlignmentNext f => GAlignmentNext (M1 i m f) where
  gAlignmentNext _ = gAlignmentNext (undefined @_ @(f _))
  {-# INLINE gAlignmentNext #-}

instance GAlignmentNext f => GAlignmentNext (f :*: g) where
  gAlignmentNext _ = gAlignmentNext (undefined @_ @(f _))
  {-# INLINE gAlignmentNext #-}

instance Storable a => GAlignmentNext (K1 i a) where
  gAlignmentNext _ = alignment (undefined @_ @a)
  {-# INLINE gAlignmentNext #-}
