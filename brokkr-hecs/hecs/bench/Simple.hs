{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Test.Tasty.Bench

import Control.DeepSeq (NFData(..))
import Control.Monad (replicateM_)

import Foreign.Storable

import GHC.Generics

import Hecs
import Hecs.Entity.Internal qualified as Hecs.EntityId

data ECSPos = ECSPos !Float !Float
  deriving stock Generic
  deriving (Storable, Component) via GenericFlat ECSPos

data ECSVel = ECSVel !Float !Float
  deriving stock Generic
  deriving (Storable, Component) via GenericFlat ECSVel

makeWorld "World" [''ECSPos, ''ECSVel]

instance NFData World where
  rnf World{} = ()
instance NFData Hecs.EntityId.FreshEntityId where
  rnf Hecs.EntityId.FreshEntityId{} = ()

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

posVelStep :: HecsM World IO ()
posVelStep = runFilter_ (filterDSL @'[ECSPos, ECSVel]) $ cmap (\(ECSPos x y, ECSVel vx vy) -> ECSPos (x + vx) (y + vy))

main :: IO ()
main = defaultMain
-- apecs massively cheats in their benchmarks. They use caches specifically of a size that always fits the 1k and 9k writes and reads
-- Whats funny is that even that doesn't save them from their atrocious component map performance, but it does provide a massive boost
-- to the arguably less important "init" benchmark because it only ever performs vector writes and never delegates to the much slower
-- uncached store (IntMap). I also have not reached the perf ceiling on the hashmap implementations although I probably won't beat the
-- init benchmark either way. Not that I even want to. Single writes outside of cmap are rare and not in the spirit of an ecs anyway and
-- an archetype based ecs just does a lot more work here. So even getting close would be amazing
  [ bgroup "pos_vel"
    [ bench "newWorld" $ whnfIO newWorld
    , bench "init" $ whnfIO (newWorld >>= \w -> runHecsM w posVelInit)
    , env (newWorld >>= \w -> runHecsM w posVelInit >> pure w) $ \w -> bench "step (filter)" $ whnfIO (runHecsM w posVelStep)
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
  ]
