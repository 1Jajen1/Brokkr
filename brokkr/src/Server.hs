module Server (
  setupServer
, Server.newWorld
, Server.runServerM
) where

import Chronos qualified
import Control.Exception
import Control.Exception.Safe qualified as SE

import Data.Coerce

import Dimension qualified

import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async

import IO.Chunkloading qualified as Chunkloading

import Network.Simple.TCP qualified as Network

import Network.Handler
import Network.Monad hiding (getUniverse)

import Control.Monad.Base
import Control.Monad.Trans.Control

import Server.Monad (Server)
import Server.Monad qualified as Server
import Server.Config

import System.JoinPlayers
import System.NetworkCommands
import System.PlayerMovement
import Data.String
import qualified System.IO as IO

setupServer :: IO () -> Server a
setupServer readyCallback = do
  cfg <- Server.getConfig

  -- set up chunkloading
  comp <- liftBase $ Chunkloading.new (configChunkloadingThreads cfg)
  Server.set (coerce $ Server.getComponentId @Chunkloading.Chunkloading) comp

  -- setup dimensions
  overworldEid <- Server.newEntity
  overworld <- Dimension.new @Dimension.Overworld (fromString $ configRootPath cfg <> "/world/region") overworldEid
  -- set this world as the overworld default
  Server.set (coerce $ Server.getComponentId @Dimension.Overworld) overworld

  --nether <- Server.newEntity
  --Dimension.new @Dimension.Overworld "./server/world/region" nether

  --theEnd <- Server.newEntity
  --Dimension.new @Dimension.TheEnd "./server/world/region" theEnd

  u <- Server.getUniverse

  liftBaseWith $ \runInBase -> Async.withAsync (do
    Network.listen (configHostPreference cfg) (configServiceName cfg) $ \(lsock, _) -> do
      readyCallback
      -- Copied from Network.Simple. I just needed a callback for when the server can accept connections
      let x :: String
          x = "Network.Simple.TCP.serve: Synchronous exception accepting connection: "
      forever $ SE.handle
        (\se -> IO.hPutStrLn IO.stderr (x ++ show (se :: SomeException)))
        . void . Network.acceptFork lsock $ \(sock, _sockAddr) -> do
          -- We want to catch any exception, even async ones.
          runNetwork cfg u sock handleConnection `catch` \(e :: SomeException) -> do
            -- TODO Ignore any crashes for now, log them later 
            -- putStrLn "Server exception"
            -- print e
            pure ()
    -- Network.serve (configHostPreference cfg) (configServiceName cfg) $ \(sock, _sockddr) -> do
    --   runNetwork cfg u sock handleConnection `catch` \(e :: SomeException) -> do
    --     -- TODO Ignore any crashes for now, log them later 
    --     -- putStrLn "Server exception"
    --     -- print e
    --     pure ()
    ) $ \as -> Async.link as >> runInBase gameLoop

gameLoop :: Server a
gameLoop = go
  where
    go = do
      start <- liftBase $ fromIntegral . Chronos.getTime <$> Chronos.now

      -- Sync all async changes to the world state
      Server.sync

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
