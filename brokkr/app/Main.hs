{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import qualified Chronos

import Data.Coerce

import qualified Dimension

import Control.Monad
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async

import qualified IO.Chunkloading as Chunkloading

import qualified Network.Simple.TCP as Network

import Network.Handler
import Network.Monad hiding (getUniverse)

import Control.Monad.Base
import Control.Monad.Trans.Control

import Server (Server, newWorld, runServerM)
import qualified Server as Hecs
import qualified Hecs

import System.JoinPlayers
import System.NetworkCommands
import System.PlayerMovement

main :: IO ()
main = newWorld >>= \u -> runServerM u $ do
  -- set up chunkloading
  comp <- liftBase $ Chunkloading.new 16 -- TODO Get from config
  Hecs.set (coerce $ Hecs.getComponentId @Chunkloading.Chunkloading) comp

  -- setup dimensions
  overworldEid <- Hecs.newEntity
  overworld <- Dimension.new @Dimension.Overworld "./server/world/region" overworldEid
  -- set this world as the overworld default
  Hecs.set (coerce $ Hecs.getComponentId @Dimension.Overworld) overworld

  --nether <- Hecs.newEntity
  --Dimension.new @Dimension.Overworld "./server/world/region" nether

  --theEnd <- Hecs.newEntity
  --Dimension.new @Dimension.TheEnd "./server/world/region" theEnd

  liftBaseWith $ \runInBase -> Async.withAsync (Network.serve (Network.Host "127.0.1.1") "25565" $ \(sock, _sockddr) -> do
  -- liftBaseWith $ \runInBase -> Async.withAsync (Network.serve (Network.Host "192.168.178.105") "25565" $ \(sock, _sockddr) -> do
    runNetwork u sock handleConnection
    ) $ \as -> Async.link as >> runInBase gameLoop

gameLoop :: Server a
gameLoop = go
  where
    go = do
      start <- liftBase $ fromIntegral . Chronos.getTime <$> Chronos.now

      -- Sync all async changes to the world state
      Hecs.sync

      -- Join new clients
      joinPlayers
      
      -- process all the stuff the clients sent
      networkCommands

      -- player block placing/breaking
      -- weather
      -- create entities
      -- world time
      -- tile events
      -- light updates
      -- create block entities
      -- weather events
      -- random events

      -- player movement -- collision?
      playerMovement

      -- update villages
      -- block events
      -- update entities
      -- update block entities

      end <- liftBase $ fromIntegral . Chronos.getTime <$> Chronos.now
      let diff = (min 0 $ end - start) `div` 1000
          tickDelay = 1000000 `div` 20 -- 20 ticks a second in microseconds
      liftBase . when (diff > tickDelay `div` 5) $ putStrLn $ "Exceeded tick delay " <> show diff
      liftBase . threadDelay $ tickDelay - diff
      go
