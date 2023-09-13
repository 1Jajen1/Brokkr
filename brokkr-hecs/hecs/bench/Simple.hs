{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
{-# OPTIONS_GHC -ddump-stg-final #-}
{-# OPTIONS_GHC -ddump-cmm #-}
{-# OPTIONS_GHC -ddump-asm #-}
module Main where

import Test.Tasty.Bench

import System.Random (mkStdGen, randomRs)

import Control.DeepSeq (NFData(..))
import Control.Monad (replicateM_)
import Control.Monad.Primitive

import Data.Coerce
import Data.Foldable (traverse_)
import Data.Primitive

import Foreign.Storable

import GHC.Generics
import GHC.Exts (RealWorld)

import Hecs
import Hecs.Filter.Internal (And)
import Hecs.Entity.Internal qualified as Hecs.EntityId

data ECSPos = ECSPos {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  deriving stock Generic
  -- deriving Component via ViaBox ECSPos
  deriving (Storable, Component) via GenericFlat ECSPos

data ECSVel = ECSVel {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  deriving stock Generic
  -- deriving Component via ViaBox ECSVel
  deriving (Storable, Component) via GenericFlat ECSVel

makeWorld "World" [''ECSPos, ''ECSVel]

instance NFData World where
  rnf World{} = ()
instance NFData (Query ty) where
  rnf !_ = ()

instance NFData (Hecs.EntityId.EntitySparseSet a) where
  rnf Hecs.EntityId.EntitySparseSet{} = () -- this is a lie, the data array is not evaluated

posVelInit :: HecsM World IO ()
posVelInit = do
  replicateM_ 1000 $ do
    eid <- newEntity
    set eid $ ECSPos 0 0
    set eid $ ECSVel 1 1
    pure ()
  replicateM_ 9000 $ do
    eid <- newEntity
    set eid $ ECSPos 0 0
    pure ()

posVelBaseline :: HecsM World IO ()
posVelBaseline = runFilter_ (filterDSL @'[ECSPos, ECSVel])
  $ cmap (\() -> ())

posVelStep :: HecsM World IO ()
posVelStep = runFilter_ (filterDSL @'[ECSPos, ECSVel])
  $ cmap (\(ECSPos x y, ECSVel vx vy) -> ECSPos (x + vx) (y + vy))

posVelStrictStep :: HecsM World IO ()
posVelStrictStep = runFilter_ (filterDSL @'[ECSPos, ECSVel])
  $ cmapM (\(ECSPos x y, ECSVel vx vy) -> let pos = ECSPos (x + vx) (y + vy) in seq pos (pure pos))

posVelBaselineQ :: Query (And ECSPos ECSVel) -> HecsM World IO ()
posVelBaselineQ q = runQuery_ q
  $ cmap (\() -> ())

posVelStepQ :: Query (And ECSPos ECSVel) -> HecsM World IO ()
posVelStepQ q = runQuery_ q
  $ cmap (\(ECSPos x y, ECSVel vx vy) -> ECSPos (x + vx) (y + vy))

posVelStrictStepQ :: Query (And ECSPos ECSVel) -> HecsM World IO ()
posVelStrictStepQ q = runQuery_ q
  $ cmapM (\(ECSPos x y, ECSVel vx vy) -> let pos = ECSPos (x + vx) (y + vy) in seq pos (pure pos))

newFloatArr :: IO (MutablePrimArray RealWorld Float)
newFloatArr = do
  arr <- newAlignedPinnedPrimArray 2000
  setPrimArray arr 0 2000 0
  pure arr

main :: IO ()
main = defaultMain
-- apecs massively cheats in their benchmarks. They use caches specifically of a size that always fits the 1k and 9k writes and reads
-- Whats funny is that even that doesn't save them from their atrocious component map performance, but it does provide a massive boost
-- to the arguably less important "init" benchmark because it only ever performs vector writes and never delegates to the much slower
-- uncached store (IntMap). I also have not reached the perf ceiling on the hashmap implementations although I probably won't beat the
-- init benchmark either way. Not that I even want to. Single writes outside of cmap are rare and not in the spirit of an ecs anyway and
-- an archetype based ecs just does a lot more work here. So even getting close would be amazing
--
-- Another note: Comparing boxed to boxed is also fun: Laziness kicks us here. Column writes are not strict for boxed components. With strictness
-- manually added it still beats apecs by 3-5x, without it it's way slower
  [ bgroup "pos_vel"
    [ bench "newWorld" $ whnfIO newWorld
    , bench "init" $ whnfIO (newWorld >>= \w -> runHecsM w posVelInit)
    , env (newWorld >>= \w -> runHecsM w posVelInit >> pure w) $ \w -> bench "step (filter) (baseline)" $ whnfIO (runHecsM w posVelBaseline)
    , env (newWorld >>= \w -> runHecsM w posVelInit >> pure w) $ \w -> bench "step (filter) (strict)" $ whnfIO (runHecsM w posVelStrictStep)
    , env (newWorld >>= \w -> runHecsM w posVelInit >> pure w) $ \w -> bench "step (filter) (lazy)" $ whnfIO (runHecsM w posVelStep)
    , env (newWorld >>= \w -> runHecsM w $ posVelInit >> query (filterDSL @'[ECSPos, ECSVel]) >>= \q -> pure (w,q)) $ \ ~(w,q) ->
        bench "step (query) (baseline)" $ whnfIO (runHecsM w (posVelBaselineQ q))
    , env (newWorld >>= \w -> runHecsM w $ posVelInit >> query (filterDSL @'[ECSPos, ECSVel]) >>= \q -> pure (w,q)) $ \ ~(w,q) ->
        bench "step (query) (strict)" $ whnfIO (runHecsM w (posVelStrictStepQ q))
    , env (newWorld >>= \w -> runHecsM w $ posVelInit >> query (filterDSL @'[ECSPos, ECSVel]) >>= \q -> pure (w,q)) $ \ ~(w,q) ->
        bench "step (query) (lazy)" $ whnfIO (runHecsM w (posVelStepQ q))
    , env ((,) <$> newFloatArr <*> newFloatArr) $ \ ~(l,r) ->
        bench "step (baseline)" $ whnfIO $ do
          let ptr1 = mutablePrimArrayContents l
              ptr2 = mutablePrimArrayContents l
          let goStep !n
                | n >= 1000 = pure ()
                | otherwise = do
                  ECSPos x y <- peekElemOff (coerce ptr1) n
                  ECSVel vx vy <- peekElemOff (coerce ptr2) n
                  pokeElemOff (coerce ptr1) n $ ECSPos (x + vx) (y + vy)
                  goStep (n + 1)
          goStep 0
          touch l
          touch r
    ]
  , bgroup "hecs"
    [ bench "newWorld" $ whnfIO newWorld
    , bench "newEntity" $ whnfIO (newWorld >>= \w -> runHecsM w (replicateM_ 10000 newEntity))
    ]
  , bgroup "hecs:EntityId"
    [ bgroup "allocateEntityId"
      [ bench "new" $ whnfIO Hecs.EntityId.new
      , bench "allocateEntityId" $ whnfIO (Hecs.EntityId.new >>= \e -> replicateM_ 10000 (Hecs.EntityId.allocateEntityId e))
      ] 
    ]
  ]

rnd, rnd' :: Int -> Int -> [Int]
rnd upper num = take num $ randomRs (0, upper) $ mkStdGen 1234
rnd' upper num = take num $ randomRs (0, upper) $ mkStdGen 4321
