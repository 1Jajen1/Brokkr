{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -ddump-cmm #-}
module Hecs.Query.Internal (
  Query(..)
, SomeQuery(..)
, createNewHandler
, addArchetype
, runQuery, runQuery#, runQuery_
, runQuery2#
) where

import Control.Monad (join)
import Control.Monad.Base
import Control.Monad.Primitive
import Control.Monad.Trans.Control

import Data.Primitive

import GHC.Exts
import GHC.IO

import Hecs.Archetype.Internal (Archetype(..), Archetype#)
import Hecs.Component.Internal
import Hecs.Filter.Internal qualified as Filter
import Hecs.Fold
import Hecs.World.Has

import Debug.Trace
import System.IO

-- TODO Also cache the bitsets? Requires observers on bitset changes on each archetype

newtype Query ty = Query SomeQuery

-- Using unlifted versions of PrimArray Int and Archetype is a small but non-zero boost for
-- the outer loop of runQuery

data SomeQuery = SomeQuery {
  newArchetypeHandler :: Archetype# -> IO ()
, cachedMatchesRef :: MutVar# RealWorld (SmallArray# Archetype#)
, cachedIndicesRef :: MutVar# RealWorld (SmallArray# ByteArray#)
}
  deriving Component via ViaBox SomeQuery

instance Has w SomeQuery where
  getComponentId _ = ComponentId . EntityId $ Bitfield 2
  {-# INLINE getComponentId #-}

addArchetype :: SomeQuery -> Archetype -> IO ()
addArchetype SomeQuery{newArchetypeHandler} (Archetype aty#) = newArchetypeHandler aty#

createNewHandler
  :: MutVar# RealWorld (SmallArray# Archetype#)
  -> MutVar# RealWorld (SmallArray# ByteArray#)
  -> Filter.Filter 'True ty
  -> Archetype#
  -> IO ()
{-# INLINE createNewHandler #-} -- Inline so that ghc creates one closure for the filter + fold
createNewHandler cachedMatchesRef cachedIndicesRef fi = \aty# -> primitive $ \s0 ->
  (# Filter.runFilter fi (Archetype aty#)
    (\(Filter.TypedArchetype _ (PrimArray inds)) acc ->
      \s ->
        case readMutVar# cachedMatchesRef s of
          (# s1, cachedMatches #) -> case readMutVar# cachedIndicesRef s1 of
            (# s2, cachedIndices #) ->
              let sz = sizeofSmallArray# cachedMatches
              in case newSmallArray# (sz +# 1#) aty# s2 of
                (# s3, newCachedMatches #) -> case copySmallArray# cachedMatches 0# newCachedMatches 0# sz s3 of
                  s4 -> case newSmallArray# (sz +# 1#) inds s4 of
                    (# s5, newCachedIndices #) -> case copySmallArray# cachedIndices 0# newCachedIndices 0# sz s5 of
                      s6 -> case unsafeFreezeSmallArray# newCachedMatches s6 of
                        (# s7, newCachedMatchesF #) -> case writeMutVar# cachedMatchesRef newCachedMatchesF s7 of
                          s8 -> case unsafeFreezeSmallArray# newCachedIndices s8 of
                            (# s9, newCachedIndicesF #) -> case writeMutVar# cachedIndicesRef newCachedIndicesF s9 of
                              s10 -> acc s10
      ) (\s -> s) s0, () #)

runQuery :: MonadBaseControl IO m => Query ty -> (Filter.TypedArchetype ty -> b -> b) -> b -> (b -> m r) -> m r
{-# INLINE runQuery #-}
runQuery (Query q) f z cont = runQuery# q f z cont

runQuery# :: MonadBaseControl IO m => SomeQuery -> (Filter.TypedArchetype ty -> b -> b) -> b -> (b -> m r) -> m r
{-# INLINE runQuery# #-}
runQuery# SomeQuery{cachedMatchesRef,cachedIndicesRef} f z cont = do
  rSt <- liftBaseWith $ \runInBase -> do
    primitive $ \s0 -> case readMutVar# cachedMatchesRef s0 of
      (# s1, cachedMatches #) -> case readMutVar# cachedIndicesRef s1 of
        (# s4, cachedIndices #) ->
          let sz = I# (sizeofSmallArray# cachedMatches)
              goQuery n@(I# n#) z
                | n >= sz = inline z -- Just as in filter, this is key when for example transforming this into a left fold
                | otherwise =
                  case indexSmallArray# cachedMatches n# of
                    (# aty #) -> case indexSmallArray# cachedIndices n# of
                      (# indices #) -> f (Filter.TypedArchetype (Archetype aty) (PrimArray indices)) (goQuery (n + 1) z)
          in case runInBase (cont (goQuery 0 z)) of IO f -> f s4
  restoreM rSt

runQuery2# :: MonadBaseControl IO m => SomeQuery -> (Filter.TypedArchetype ty -> b -> b) -> b -> (b -> m r) -> m r
{-# INLINE runQuery2# #-}
runQuery2# SomeQuery{cachedMatchesRef,cachedIndicesRef} f z cont = do
  rSt <- liftBaseWith $ \runInBase -> do
    primitive $ \s0 -> case readMutVar# cachedMatchesRef s0 of
      (# s1, cachedMatches #) -> case readMutVar# cachedIndicesRef s1 of
        (# s4, cachedIndices #) ->
          let sz = I# (sizeofSmallArray# cachedMatches)
              goQuery n@(I# n#) z
                | n >= sz = inline z -- Just as in filter, this is key when for example transforming this into a left fold
                | otherwise =
                  case indexSmallArray# cachedMatches n# of
                    (# aty #) -> case indexSmallArray# cachedIndices n# of
                      (# indices #) -> f (Filter.TypedArchetype (Archetype aty) (PrimArray indices)) (goQuery (n + 1) z)
          in case runInBase (cont (goQuery 0 z)) of IO f -> f s4
  restoreM rSt

data Box s (a :: TYPE (BoxedRep Unlifted)) = Box (# State# s, a #)

traceFlush str a = unsafePerformIO $ do
  putStrLn str
  hFlush stdout
  pure a

runQuery_
  :: forall w ty m a b z
  . ( MonadBaseControl IO m
    , Filter.FoldBitSets ty
    , HasColumns w ty a, ReadColumns ty a
    , HasColumns w ty b, WriteColumns ty b
    )
  => Query ty -> FoldM m a b z -> m z
{-# INLINE runQuery_ #-}
runQuery_ q fo =
  toEntityFold @w fo $ \f z e -> do
    runQuery q (\tyAty acc -> oneShot $ \z' -> f z' tyAty >>= acc) (oneShot pure) $ \f -> z >>= f >>= e
