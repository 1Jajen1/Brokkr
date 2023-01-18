{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import qualified Chronos

import Data.Coerce
import Data.Foldable (forM_)

import qualified Dimension

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async

import qualified IO.Chunkloading as Chunkloading

import qualified Network.Connection as Conn

import qualified Network.Simple.TCP as Network

import Network.Handler
import Network.Monad hiding (getUniverse)

import Control.Monad.Base
import Control.Monad.Trans.Control

import Monad (Server, newWorld, runServerM)
import qualified Monad as Hecs

import Client (Joined)

import System.JoinPlayers

main :: IO ()
main = newWorld >>= \u -> runServerM u $ do
  -- set up chunkloading
  comp <- liftBase $ Chunkloading.new 16 -- TODO Get from config
  Hecs.setComponent (coerce $ Hecs.getComponentId @Chunkloading.Chunkloading) comp

  -- setup dimensions
  overworldEid <- Hecs.newEntity
  overworld <- Dimension.new @Dimension.Overworld "./server/world/region" overworldEid
  -- set this world as the overworld default
  Hecs.setComponent (coerce $ Hecs.getComponentId @Dimension.Overworld) overworld

  --nether <- Hecs.newEntity
  --Dimension.new @Dimension.Overworld "./server/world/region" nether

  --theEnd <- Hecs.newEntity
  --Dimension.new @Dimension.TheEnd "./server/world/region" theEnd

  liftBaseWith $ \runInBase -> Async.withAsync (Network.serve (Network.Host "192.168.178.105") "25565" $ \(sock, _sockddr) -> do
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
      
      -- process all client changes
      -- iterate all connections with Joined
      -- TODO Change to query.
      Hecs.filter (Hecs.filterDSL @'[Conn.Connection, Joined])
        (\aty _ -> do
          connections <- Hecs.getColumn @Conn.Connection aty
          Hecs.iterateArchetype aty $ \n eid -> do
            conn <- Hecs.readStored connections n
            -- liftBase $ print eid
            pure ()
            )
        (pure ())

      {- OLD

      -- Right here we have all packets executed, so everything from the client has been done
      -- Now we need to process these changes

      -- update the world
      -- perform chunk updates (ie mutate the actual data array and create final chunk update packet)

        -- Iterate over all worlds (yes just store them in an array)
          -- call update
            -- iterate all (loaded) chunks
              -- call update
                -- iterate all chunksections, perform all block changes
                -- combine all chunksection blockchanges into a change packet

        -- update entities (do this in the per world part)
        -- perform movement changes and create the relevant packets

      -- update clients (done after world and entities so that all chunks and entities have their change packets attached)
      -- lots of things, movement, updating loaded chunks ...
        -- also grab changes from loaded chunks and entities. They are already calculated, just need to put them in our send queue
  -}

      end <- liftBase $ fromIntegral . Chronos.getTime <$> Chronos.now
      let diff = (min 0 $ end - start) `div` 1000
          tickDelay = 1000000 `div` 20 -- 20 ticks a second in microseconds
      liftBase . threadDelay $ tickDelay - diff
      go
