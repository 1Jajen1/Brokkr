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

import Control.Monad.Trans.Control


import Hecs.Filter
import Data.Kind
import Control.Monad.Base
import Hecs.World.Has
import Hecs.Entity.Internal
import Hecs.Component.Internal
import Data.Proxy
import Language.Haskell.TH qualified as TH
import Control.Monad (replicateM)
import GHC.TypeLits

-- Generates
--
-- type family Cols a :: Type where
--   Cols (a1,...,aN) = (Cols a1,...,Cols aN)
--   Cols () = ()
--   Cols EntityId = Column Flat EntityId
--   Cols a  = Column (ComponentKind a) a
--
-- instance HasColumns w ty (a1,...,aN) where
--   columnsFor aty = (,...,) <$> columnsFor @w @ty @a1 aty <*> ... <*> columnsFor @w @ty @aN aty
-- instance ReadColumns ty (a1,...,aN) where
--   readCols (col1,...,colN) n = (,...,) <$> readCols col1 n <*> ... <*> readCols colN n
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
      -- append Cols a = Column (ComponentKind a) a
      <>  [ TH.tySynEqn
              Nothing
              (TH.appT (TH.conT (TH.mkName "Cols")) (TH.tupleT 0))
              (TH.tupleT 0)
          , TH.tySynEqn
              Nothing
              (TH.appT (TH.conT (TH.mkName "Cols")) (TH.conT (TH.mkName "EntityId")))
              (TH.appT (TH.appT (TH.conT (TH.mkName "Column")) (TH.conT (TH.mkName "Flat"))) (TH.conT (TH.mkName "EntityId")))
          , TH.newName "a" >>= \aName -> TH.tySynEqn
              (Just [TH.plainTV aName])
              (TH.appT (TH.conT (TH.mkName "Cols")) (TH.varT aName))
              (TH.appT (TH.appT (TH.conT (TH.mkName "Column")) (TH.appT (TH.conT (TH.mkName "ComponentKind")) (TH.varT aName))) (TH.varT aName))
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
        [ replicateM n (TH.newName "col") >>= \colsNames -> TH.newName "ind" >>= \indName -> TH.funD
            (TH.mkName "readCols")
            [TH.clause [TH.tupP $ fmap TH.varP colsNames, TH.varP indName] (TH.normalB (
                let (n0,c0):nxs = zip names colsNames
                in foldr
                  (\(n, colN) acc -> TH.appE (TH.appE (TH.varE (TH.mkName "<*>")) acc) (TH.appE (TH.appE (TH.appTypeE (TH.appTypeE (TH.varE (TH.mkName "readCols")) (TH.varT tyName)) (TH.varT n)) (TH.varE colN)) (TH.varE indName)))
                  (TH.appE (TH.appE (TH.varE (TH.mkName "<$>")) (pure $ TH.TupE $ fmap (const Nothing) names)) (TH.appE (TH.appE (TH.appTypeE (TH.appTypeE (TH.varE (TH.mkName "readCols")) (TH.varT tyName)) (TH.varT n0)) (TH.varE c0)) (TH.varE indName)))
                  $ reverse nxs
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
  columnsFor :: MonadBase IO m => TypedArchetype ty -> m (Cols a)

instance {-# OVERLAPPING #-} HasColumns w ty () where
  columnsFor _ = pure ()
  {-# INLINE columnsFor #-}

instance {-# OVERLAPPING #-} HasColumns w ty EntityId where
  columnsFor = getEntityColumn
  {-# INLINE columnsFor #-}

instance {-# OVERLAPPABLE #-} (Has w a, Component a, TypedHas ty a, Cols a ~ Column (ComponentKind a) a) => HasColumns w ty a where 
  columnsFor aty = getColumnWithId aty (getComponentId (Proxy @w))
  {-# INLINE columnsFor #-}

class ReadColumns ty a where
  readCols :: MonadBase IO m => Cols a -> Int -> m a

instance {-# OVERLAPPING#-} ReadColumns ty () where
  readCols _ _ = pure ()
  {-# INLINE readCols #-}

instance {-# OVERLAPPING #-} ReadColumns ty EntityId where
  readCols = readColumn
  {-# INLINE readCols #-}

instance {-# OVERLAPPABLE #-} (Cols a ~ Column (ComponentKind a) a, AccessColumn (ComponentKind a) a) => ReadColumns ty a where 
  readCols = readColumn
  {-# INLINE readCols #-}

class WriteColumns ty a where
  writeCols :: MonadBase IO m => Cols a -> Int -> a -> m ()

instance {-# OVERLAPPING #-} WriteColumns ty () where
  writeCols _ _ _ = pure ()
  {-# INLINE writeCols #-}

instance {-# OVERLAPPING #-} TypeError ('Text "Cannot write an EntityId like a component") => WriteColumns ty EntityId where
  writeCols = error "Unreachable"
  {-# INLINE writeCols #-}

instance {-# OVERLAPPABLE #-} (Cols a ~ Column (ComponentKind a) a, AccessColumn (ComponentKind a) a) => WriteColumns ty a where 
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
data FoldM m a b z = forall x . FoldM (x -> a -> m (b, x)) (m x) (x -> m z)

-- TODO: This first implementation is almost equal to what foldl does with the added b output
--       Experiment with other representations that might fit better

-- Note: The inline pragmas may seem excessive (and they are) but 'FoldM' requires ghc to unbox and inline
--       as much as possible to get decent performance, so this is playing it safe

instance Functor m => Functor (FoldM m a b) where
  fmap f (FoldM step initial extract) = FoldM step initial (fmap f . extract)
  {-# INLINE fmap #-}

-- | Map over a set of components, writing to another set
--
-- If you need monad effects, see 'cmapM'.
cmap :: Applicative m => (a -> b) -> FoldM m a b ()
cmap f = FoldM (\() a -> pure (f a, ())) (pure ()) pure
{-# INLINE cmap #-}

-- | Map monadically over a set of components, writing to another set
--
-- If you don't need to write components, see 'cmapM_'.
cmapM :: Applicative m => (a -> m b) -> FoldM m a b ()
cmapM f = FoldM (\() a -> (,()) <$> f a) (pure ()) pure
{-# INLINE cmapM #-}

-- | Fold monadically over a set of components without writing or accumulating
--
-- If you do need to write components, see 'cmapM'.
-- If you want to accumulate a result see 'cfold' or 'cfoldM'
cmapM_ :: Applicative m => (a -> m ()) -> FoldM m a () ()
cmapM_ = cmapM
{-# INLINE cmapM_ #-}

-- | Fold over a set of components, accumulating a result
--
-- If you don't need a result, see 'cmapM_'.
-- If you need monadic effects, see 'cfoldM'.
cfold :: Monad m => (x -> a -> x) -> x -> FoldM m a () x
cfold f z = cfoldM (\acc a -> pure $ f acc a) (pure z)
{-# INLINE cfold #-}

-- | Fold monadically over a set of components, accumulating a result
--
-- If you don't need a result, see 'cmapM_'.
-- If you don't need monadic effects, see 'cfold'.
cfoldM :: Monad m => (x -> a -> m x) -> m x -> FoldM m a () x
cfoldM f z = FoldM (\acc a -> ((),) <$> f acc a) z pure
{-# INLINE cfoldM #-}

-- | Eliminate a 'FoldM' and turn it into a fold over archetypes
toEntityFold :: forall w ty a b z m r
  . ( MonadBaseControl IO m
    , HasColumns w ty a, ReadColumns ty a
    , HasColumns w ty b, WriteColumns ty b
    )
  => FoldM m a b z
  -> (forall x . (x -> TypedArchetype ty -> m x) -> m x -> (x -> m z) -> r)
  -> r
toEntityFold (FoldM step initial extract) f = f
  (\x0 ty -> do
    inpCols <- columnsFor @w @_ @a ty
    outCols <- columnsFor @w @_ @b ty
    iterateArchetype ty
      (\n _ x -> do
        a <- readCols @ty inpCols n
        (b, x') <- step x a
        writeCols @ty outCols n b
        pure x'
        )
      (pure x0)
    ) initial extract
{-# INLINE toEntityFold #-}
