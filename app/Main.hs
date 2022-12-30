{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import qualified Chronos

import qualified Command
import qualified Command.Handler as Command

import Data.Foldable (forM_)

import qualified Dimension

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async

import qualified Network.Connection as Conn

import qualified Network.Simple.TCP as Network

import Network.Handler
import Network.Monad

import Client (connection)
import qualified Client

import Server (Server)
import qualified Server

main :: IO ()
main = do
  server <- Server.new
  Async.withAsync (Network.serve (Network.Host "192.168.178.59") "25565" $ \(sock, _sockddr) -> do
    runNetwork server sock handleConnection
    ) $ \as -> Async.link as >> gameLoop server

gameLoop :: Server -> IO a
gameLoop server = go
  where
    go = do
      start <- fromIntegral . Chronos.getTime <$> Chronos.now
      
      -- handle all packets send by clients
      Client.withClients (Server.connectedClients server) $ \connections ->
        -- TODO Better concurrency strategy?
        Async.forConcurrently_ connections $ \p -> do
          Conn.flushCommands (connection p) $ \cs -> forM_ cs $ \c -> do
            Command.runHandler server $ Command.handle p c

      -- Right here we have all packets executed, so everything from the client has been done
      -- Now we need to process these changes

      -- update the world
      -- perform chunk updates (ie mutate the actual data array and create final chunk update packet)

      Dimension.withDimensions (Server.dimensions server) $ \dims -> Async.forConcurrently_ dims $ \dim ->
        pure ()
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

      Client.withClients (Server.connectedClients server) $ \connections -> Async.forConcurrently_ connections $ \conn ->
        Client.update conn

      end <- fromIntegral . Chronos.getTime <$> Chronos.now
      let diff = (min 0 $ end - start) `div` 1000
          tickDelay = 1000000 `div` 20 -- 20 ticks a second in microseconds
      threadDelay $ tickDelay - diff
      go
