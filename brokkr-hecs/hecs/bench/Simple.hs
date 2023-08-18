{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Test.Tasty.Bench

import System.Random (mkStdGen, randomRs)

import Control.DeepSeq (NFData(..))
import Control.Monad (replicateM_)

import Data.Foldable (traverse_)

import Foreign.Storable

import GHC.Generics

import Hecs
import Hecs.Entity.Internal qualified as Hecs.EntityId
import Hecs.HashTable.Boxed qualified

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

instance NFData Hecs.EntityId.FreshEntityId where
  rnf Hecs.EntityId.FreshEntityId{} = ()
instance NFData (Hecs.HashTable.Boxed.HashTable k v) where
  rnf Hecs.HashTable.Boxed.HashTable{} = () -- TODO This is a lie. v is not evaluated

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
posVelBaseline = runFilter_ (filterDSL @'[ECSPos, ECSVel]) $ cmap (\() -> ())

posVelStep :: HecsM World IO ()
posVelStep = runFilter_ (filterDSL @'[ECSPos, ECSVel]) $ cmap (\(ECSPos x y, ECSVel vx vy) -> ECSPos (x + vx) (y + vy))

posVelStrictStep :: HecsM World IO ()
posVelStrictStep = runFilter_ (filterDSL @'[ECSPos, ECSVel]) $ cmapM (\(ECSPos x y, ECSVel vx vy) -> let pos = ECSPos (x + vx) (y + vy) in seq pos (pure pos))

main :: IO ()
main = defaultMain
-- apecs massively cheats in their benchmarks. They use caches specifically of a size that always fits the 1k and 9k writes and reads
-- Whats funny is that even that doesn't save them from their atrocious component map performance, but it does provide a massive boost
-- to the arguably less important "init" benchmark because it only ever performs vector writes and never delegates to the much slower
-- uncached store (IntMap). I also have not reached the perf ceiling on the hashmap implementations although I probably won't beat the
-- init benchmark either way. Not that I even want to. Single writes outside of cmap are rare and not in the spirit of an ecs anyway and
-- an archetype based ecs just does a lot more work here. So even getting close would be amazing
--
-- Another note: Comparing boxed to boxed is also fun: But laziness kicks us here. Column writes are not strict for boxed components. With strictness
-- it still beats apecs by 3-5x, without it it's way slower
  [ bgroup "pos_vel"
    [ bench "newWorld" $ whnfIO newWorld
    , bench "init" $ whnfIO (newWorld >>= \w -> runHecsM w posVelInit)
    , env (newWorld >>= \w -> runHecsM w posVelInit >> pure w) $ \w -> bench "step (filter) (baseline)" $ whnfIO (runHecsM w posVelBaseline)
    , env (newWorld >>= \w -> runHecsM w posVelInit >> pure w) $ \w -> bench "step (filter) (strict)" $ whnfIO (runHecsM w posVelStrictStep)
    , env (newWorld >>= \w -> runHecsM w posVelInit >> pure w) $ \w -> bench "step (filter) (lazy)" $ whnfIO (runHecsM w posVelStep)
    ]
  , bgroup "hecs"
    [ bench "newWorld" $ whnfIO newWorld
    , bench "newEntity" $ whnfIO (newWorld >>= \w -> runHecsM w (replicateM_ 9000 newEntity))
    ]
  , bgroup "hecs:EntityId"
    [ bgroup "allocateEntityId"
      [ bench "baseline" $ whnfIO Hecs.EntityId.new
      , bench "allocateEntityId" $ whnfIO (Hecs.EntityId.new >>= \e -> replicateM_ 9000 (Hecs.EntityId.allocateEntityId e))
      ] 
    ]
  , bgroup "hecs:hashtable"
    [ env (Hecs.HashTable.Boxed.new 32 >>= \table -> traverse_ (\k -> Hecs.HashTable.Boxed.insert table k k) (rnd 20000 10000) >> pure (table, rnd' 20000 10000))
        $ \ ~(table, items) -> bench "lookup (many)" $ whnfIO (traverse_ (Hecs.HashTable.Boxed.lookup table) items)
    , env (Hecs.HashTable.Boxed.new 32 >>= \table -> traverse_ (\k -> Hecs.HashTable.Boxed.insert table k k) (rnd 20000 10000) >> pure (table, rnd' 20000 10000))
        $ \ ~(table, items) -> bench "insert (many)" $ whnfIO (traverse_ (\k -> Hecs.HashTable.Boxed.insert table k k) items)
    ]
  ]

rnd, rnd' :: Int -> Int -> [Int]
rnd upper num = take num $ randomRs (0, upper) $ mkStdGen 1234
rnd' upper num = take num $ randomRs (0, upper) $ mkStdGen 4321
