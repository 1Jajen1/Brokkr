{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Hecs.Fold (
  FoldM
, cmap, cmapM, cmapM_
, cfold, cfoldM
-- Internals
, toEntityFold
, HasColumns(..)
, ReadColumns(..)
, WriteColumns(..)
) where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Control.Monad.Base
import Control.Monad.Trans.Control

import Data.Coerce
import Data.Kind
import Data.Proxy

import GHC.Exts (RealWorld, oneShot)
import GHC.TypeLits

import Hecs.Component.Column
import Hecs.Component.Internal
import Hecs.Entity.Internal
import Hecs.Filter
import Hecs.World.Has

import Language.Haskell.TH qualified as TH

-- TODO Maybe columns (read only access?)

-- Generates
--
-- type family Cols a :: Type where
--   Cols (a1,...,aN) = (Cols a1,...,Cols aN)
--   Cols () = ()
--   Cols EntityId = Column Flat EntityId
--   Cols (ComponentId c) = (ComponentId c)
--   Cols a  = Column (ComponentKind a) a
--
-- instance HasColumns w ty (a1,...,aN) where
--   columnsFor aty = (,...,) <$> columnsFor @w @ty @a1 aty <*> ... <*> columnsFor @w @ty @aN aty
-- instance ReadColumns ty (a1,...,aN) where
--   readCols (col1,...,colN) n cont = readCols col1 n $ \a1 -> ... readCols colN n $ \aN -> cont (a1,...,aN)
-- instance WriteColumns ty (a1,...,aN) where
--   writeCols (col1,...,colN) n (a1,...,aN) = writeCols col1 n a1 >> ... >> writeCols colN n aN
--
$(do 
  let numTuples = 16
  tyFam <- TH.closedTypeFamilyD (TH.mkName "Cols") [TH.plainTV (TH.mkName "a")] (TH.kindSig (TH.ConT (TH.mkName "Type"))) Nothing $ (do
      n <- [2..numTuples]
      pure $ replicateM n (TH.newName "x") >>= \names -> TH.tySynEqn
        (Just $ fmap TH.plainTV names)
        (TH.appT (TH.conT (TH.mkName "Cols")) (foldr (\n acc -> TH.appT acc (TH.varT n)) (TH.tupleT n) names))
        (foldr (\n acc -> TH.appT acc (TH.appT (TH.conT (TH.mkName "Cols")) (TH.varT n))) (TH.tupleT n) names)
    ) -- append Cols () = ()
      -- append Cols EntityId = Column Flat EntityId
      -- append Cols (ComponentId c) = ComponentId c
      -- append Cols a = Column (ComponentKind a) a
      <>  [ TH.tySynEqn
              Nothing
              (TH.appT (TH.conT (TH.mkName "Cols")) (TH.tupleT 0))
              (TH.tupleT 0)
          , TH.tySynEqn
              Nothing
              (TH.appT (TH.conT (TH.mkName "Cols")) (TH.conT (TH.mkName "EntityId")))
              (TH.appT (TH.appT (TH.appT (TH.conT (TH.mkName "Column")) (TH.conT (TH.mkName "Flat"))) (TH.conT (TH.mkName "RealWorld"))) (TH.conT (TH.mkName "EntityId")))
          , TH.newName "c" >>= \aName -> TH.tySynEqn
              (Just [TH.plainTV aName])
              (TH.appT (TH.conT (TH.mkName "Cols")) (TH.appT (TH.conT (TH.mkName "ComponentId")) (TH.varT aName)))
              (TH.appT (TH.conT (TH.mkName "ComponentId")) (TH.varT aName))
          , TH.newName "a" >>= \aName -> TH.tySynEqn
              (Just [TH.plainTV aName])
              (TH.appT (TH.conT (TH.mkName "Cols")) (TH.varT aName))
              (TH.appT (TH.appT (TH.appT (TH.conT (TH.mkName "Column")) (TH.appT (TH.conT (TH.mkName "ComponentKindFor")) (TH.varT aName))) (TH.conT (TH.mkName "RealWorld"))) (TH.varT aName))
          ]
  
  hasColInstances <- sequence $ do
    n <- [2..numTuples]
    pure $ replicateM n (TH.newName "x") >>= \names -> TH.newName "ty" >>= \tyName -> TH.newName "w" >>= \wName -> do
      TH.instanceD
        (traverse (TH.appT (TH.appT (TH.appT (TH.conT (TH.mkName "HasColumns")) (TH.varT wName)) (TH.varT tyName)) . TH.varT) names)
        (TH.appT (TH.appT (TH.appT (TH.conT (TH.mkName "HasColumns")) (TH.varT wName)) (TH.varT tyName)) $ foldr (\n acc -> TH.appT acc (TH.varT n)) (TH.tupleT n) $ reverse names)
        [ TH.newName "aty" >>= \atyName -> TH.funD
            (TH.mkName "columnsFor")
            [TH.clause [TH.varP atyName] (TH.normalB (
                let n0:nxs = names
                in foldr
                  (\n acc -> TH.appE (TH.appE (TH.varE (TH.mkName "<*>")) acc) (TH.appE (TH.appTypeE (TH.appTypeE (TH.appTypeE (TH.varE (TH.mkName "columnsFor")) (TH.varT wName)) (TH.varT tyName)) (TH.varT n)) (TH.varE atyName)))
                  (TH.appE (TH.appE (TH.varE (TH.mkName "<$>")) (pure $ TH.TupE $ fmap (const Nothing) names)) (TH.appE (TH.appTypeE (TH.appTypeE (TH.appTypeE (TH.varE (TH.mkName "columnsFor")) (TH.varT wName)) (TH.varT tyName)) (TH.varT n0)) $ TH.varE atyName))
                  $ reverse nxs
              )) []]
          , TH.pragInlD (TH.mkName "columnsFor") TH.Inline TH.FunLike TH.AllPhases
          ]
  
  readColInstances <- sequence $ do
    n <- [2..numTuples]
    pure $ replicateM n (TH.newName "x") >>= \names -> TH.newName "ty" >>= \tyName -> do
      TH.instanceD
        (traverse (TH.appT (TH.appT (TH.conT (TH.mkName "ReadColumns")) (TH.varT tyName)) . TH.varT) names)
        (TH.appT (TH.appT (TH.conT (TH.mkName "ReadColumns")) (TH.varT tyName)) $ foldr (\n acc -> TH.appT acc (TH.varT n)) (TH.tupleT n) $ reverse names)
        [ replicateM n (TH.newName "a") >>= \varNames -> replicateM n (TH.newName "col") >>= \colsNames -> TH.newName "ind" >>= \indName -> TH.newName "cont" >>= \contName -> TH.funD
            (TH.mkName "readCols")
            [TH.clause [TH.tupP $ fmap TH.varP colsNames, TH.varP indName, TH.varP contName] (TH.normalB (
                let (n0,c0):nxs = zip names colsNames
                in foldr
                  (\(vN, (n, colN)) acc ->
                      TH.appE
                        (TH.appE
                          (TH.appE
                            (TH.appTypeE
                              (TH.appTypeE
                                (TH.varE (TH.mkName "readCols"))
                                (TH.varT tyName))
                              (TH.varT n))
                            (TH.varE colN))
                          (TH.varE indName))
                        (TH.lamE [TH.varP vN] $ acc))
                  (TH.appE (TH.varE contName) $ pure $ TH.TupE $ fmap (Just . TH.VarE) varNames)
                  $ zip varNames (zip names colsNames) -- reverse nxs
              )) []]
          , TH.pragInlD (TH.mkName "readCols") TH.Inline TH.FunLike TH.AllPhases
          ]
  
  writeColInstances <- sequence $ do
    n <- [2..numTuples]
    pure $ replicateM n (TH.newName "x") >>= \names -> TH.newName "ty" >>= \tyName -> do
      TH.instanceD
        (traverse (TH.appT (TH.appT (TH.conT (TH.mkName "WriteColumns")) (TH.varT tyName)) . TH.varT) names)
        (TH.appT (TH.appT (TH.conT (TH.mkName "WriteColumns")) (TH.varT tyName)) $ foldr (\n acc -> TH.appT acc (TH.varT n)) (TH.tupleT n) $ reverse names)
        [ replicateM n (TH.newName "col") >>= \colsNames -> TH.newName "ind" >>= \indName -> replicateM n (TH.newName "a") >>= \aNames -> TH.funD
            (TH.mkName "writeCols")
            [TH.clause [TH.tupP $ fmap TH.varP colsNames, TH.varP indName, TH.tupP $ fmap TH.varP aNames] (TH.normalB (
                let ((n0,c0),a0):nxs = zip (zip names colsNames) aNames
                in foldr
                  (\((n, colN), aN) acc -> TH.appE (TH.appE (TH.varE (TH.mkName ">>")) acc) (TH.appE (TH.appE (TH.appE (TH.appTypeE (TH.appTypeE (TH.varE (TH.mkName "writeCols")) (TH.varT tyName)) (TH.varT n)) (TH.varE colN)) (TH.varE indName)) (TH.varE aN)))
                  (TH.appE (TH.appE (TH.appE (TH.appTypeE (TH.appTypeE (TH.varE (TH.mkName "writeCols")) (TH.varT tyName)) (TH.varT n0)) (TH.varE c0)) (TH.varE indName)) (TH.varE a0))
                  $ reverse nxs
              )) []]
          , TH.pragInlD (TH.mkName "writeCols") TH.Inline TH.FunLike TH.AllPhases
          ]
  
  pure $ [tyFam] ++ hasColInstances ++ readColInstances ++ writeColInstances
  )

class HasColumns (w :: Type) (ty :: Type) (a :: Type) where
  columnsFor :: TypedArchetype ty -> IO (Cols a)

instance {-# OVERLAPPING #-} HasColumns w ty () where
  columnsFor _ = pure ()
  {-# INLINE columnsFor #-}

instance {-# OVERLAPPING #-} HasColumns w ty EntityId where
  columnsFor aty = withEntityIdColumn aty pure
  {-# INLINE columnsFor #-}

instance {-# OVERLAPPING #-} (TypedHas ty c, KnownNat (FirstMatch ty c (TypeMatches ty c 0))) => HasColumns w ty (ComponentId c) where
  columnsFor aty = withComponentId @c aty pure
  {-# INLINE columnsFor #-}

instance {-# OVERLAPPABLE #-} (Component a, TypedHas ty a, KnownNat (FirstMatch ty a (TypeMatches ty a 0)), Cols a ~ Column (ComponentKindFor a) RealWorld a) => HasColumns w ty a where 
  columnsFor aty = withColumn @a aty pure
  {-# INLINE columnsFor #-}

class ReadColumns ty a where
  readCols :: Cols a -> Int -> (a -> IO r) -> IO r

instance {-# OVERLAPPING#-} ReadColumns ty () where
  readCols _ _ cont = cont ()
  {-# INLINE readCols #-}

instance {-# OVERLAPPING #-} ReadColumns ty EntityId where
  readCols = readColumn
  {-# INLINE readCols #-}

instance {-# OVERLAPPING #-} ReadColumns ty (ComponentId c) where
  readCols i _ cont = cont i 
  {-# INLINE readCols #-}

instance {-# OVERLAPPABLE #-} (Component a, Cols a ~ Column (ComponentKindFor a) RealWorld a, Coercible (ComponentValueFor a) a) => ReadColumns ty a where 
  readCols = readColumn
  {-# INLINE readCols #-}

class WriteColumns ty a where
  writeCols :: Cols a -> Int -> a -> IO ()

instance {-# OVERLAPPING #-} WriteColumns ty () where
  writeCols _ _ _ = pure ()
  {-# INLINE writeCols #-}

instance {-# OVERLAPPABLE #-} (Component a, Cols a ~ Column (ComponentKindFor a) RealWorld a, Coercible (ComponentValueFor a) a) => WriteColumns ty a where 
  writeCols = writeColumn
  {-# INLINE writeCols #-}

-- | Fold over a set of components a, writing another set of components b while producing an accumulator
--
-- High level datastructure when processing filters/queries/systems
--
-- This convenience comes at a cost:
-- Using () and tuples as components is no longer supported, as those have been given special meaning here.
-- Specifically tuples represent reading or writing to multiple components and unit represents not writing
-- or reading in the first place.
--
-- Note:
-- You can process a single table with multiple folds at the same time by using the 'Applicative' instance,
-- however the fold is then required to merge the output 'b' produced by each fold somehow. This is trivial
-- for folds that don't write (ie use '()'), but may be problematic for others.
-- All folds must read and write the same components (not necessarily use, but they have to have the same type).
data FoldM m a b z = forall x . FoldM (x -> a -> m (b, x)) (m x) (x -> m z)

-- TODO: This first implementation is almost equal to what foldl does with the added b output
--       Experiment with other representations that might fit better

-- Note: The inline pragmas may seem excessive (and they are) but 'FoldM' requires ghc to unbox and inline
--       as much as possible to get decent performance, so this is playing it safe

instance Functor m => Functor (FoldM m a b) where
  fmap f (FoldM step initial extract) = FoldM step initial (fmap f . extract)
  {-# INLINE fmap #-}

instance (Monoid b, Applicative m) => Applicative (FoldM m a b) where
  pure a = FoldM (\_ _ -> pure (mempty, ())) (pure ()) (\_ -> pure a)
  {-# INLINE pure #-}
  liftA2 f (FoldM lStep lInitial lExtract) (FoldM rStep rInitial rExtract) =
    FoldM
      (\(x1,x2) a -> liftA2 (\(b1,x1') (b2,x2') -> (b1 <> b2, (x1',x2'))) (lStep x1 a) (rStep x2 a))
      ((,) <$> lInitial <*> rInitial)
      (\(x1,x2) -> liftA2 f (lExtract x1) (rExtract x2))
  {-# INLINE liftA2 #-}

-- | Monadically map over the final result of the fold
--
-- Useful because 'FoldM' cannot not have a monad instance
after :: Monad m => FoldM m a b z -> (z -> m z') -> FoldM m a b z'
{-# INLINE after #-}
after (FoldM step initial extract) f = FoldM step initial (\x -> extract x >>= f)


-- | Map over a set of components, writing to another set
--
-- If you need monad effects, see 'cmapM'.
cmap :: Applicative m => (a -> b) -> FoldM m a b ()
{-# INLINE cmap #-}
cmap f = FoldM (\() a -> pure (f a, ())) (pure ()) pure

-- | Map monadically over a set of components, writing to another set
--
-- If you don't need to write components, see 'cmapM_'.
cmapM :: Applicative m => (a -> m b) -> FoldM m a b ()
{-# INLINE cmapM #-}
cmapM f = FoldM (\() a -> (,()) <$> f a) (pure ()) pure

-- | Fold monadically over a set of components without writing or accumulating
--
-- If you do need to write components, see 'cmapM'.
-- If you want to accumulate a result see 'cfold' or 'cfoldM'
cmapM_ :: Applicative m => (a -> m ()) -> FoldM m a () ()
{-# INLINE cmapM_ #-}
cmapM_ = cmapM

-- | Fold over a set of components, accumulating a result
--
-- If you don't need a result, see 'cmapM_'.
-- If you need monadic effects, see 'cfoldM'.
cfold :: Monad m => (x -> a -> x) -> x -> FoldM m a () x
{-# INLINE cfold #-}
cfold f z = cfoldM (\acc a -> pure $ f acc a) (pure z)

-- | Fold monadically over a set of components, accumulating a result
--
-- If you don't need a result, see 'cmapM_'.
-- If you don't need monadic effects, see 'cfold'.
cfoldM :: Monad m => (x -> a -> m x) -> m x -> FoldM m a () x
{-# INLINE cfoldM #-}
cfoldM f z = FoldM (\acc a -> ((),) <$> f acc a) z pure

-- | Eliminate a 'FoldM' and turn it into a fold over archetypes
toEntityFold :: forall w ty a b z m r
  . ( MonadBaseControl IO m
    , FoldBitSets ty
    , HasColumns w ty a, ReadColumns ty a
    , HasColumns w ty b, WriteColumns ty b
    )
  => FoldM m a b z
  -> (forall x . (x -> TypedArchetype ty -> m x) -> m x -> (x -> m z) -> r)
  -> r
{-# INLINE toEntityFold #-}
toEntityFold (FoldM step initial extract) = oneShot $ \f -> f
  (\x0 ty -> do
    !inpCols <- liftBase $ columnsFor @w @_ @a ty
    !outCols <- liftBase $ columnsFor @w @_ @b ty
    st <- liftBaseWith $ \runInBase -> do
      foldTypedArchetype ty
        (\n st -> readCols @ty inpCols n $ \a ->
          runInBase $ do
            x <- restoreM st
            (b, x') <- step x a
            liftBase $ writeCols @ty outCols n b
            pure x'
          )
        (runInBase $ pure x0)
    restoreM st
    ) initial extract
