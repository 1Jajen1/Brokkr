module Brokkr.Server (
  setupServer
, Server.newWorld
, Server.runServerM
) where

import Brokkr.Debug.Monad

import Brokkr.Dimension qualified as Dimension

import Brokkr.IO.Chunkloading qualified as Chunkloading

import Brokkr.Network.Handler
import Brokkr.Network.Monad hiding (getUniverse)

import Brokkr.Server.Config
import Brokkr.Server.Monad (Server)
import Brokkr.Server.Monad qualified as Server

import Brokkr.System.JoinPlayers qualified as System
import Brokkr.System.PlayerMovement qualified as System
import Brokkr.System.NetworkCommands qualified as System

import Chronos qualified

import Control.Exception
import Control.Exception.Safe qualified as SE

import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async

import Data.Coerce
import Data.String

import Network.Simple.TCP qualified as Network

import System.IO qualified as IO

setupServer :: IO () -> Server a
setupServer readyCallback = do
  cfg <- Server.getConfig

  -- set up chunkloading
  -- TODO Even this should be an entity. This way we can add system which manipulate stuff later on
  -- TODO Then even chunks should be entities. Makes lifecycles easier right?
  --      Also we can have systems over chunks, worlds etc.
  chunkloading <- liftBase $ Chunkloading.new (configChunkloadingThreads cfg)
  -- Server.set (coerce $ Server.getComponentId @Chunkloading.Chunkloading) comp

  -- setup dimensions
  overworldEid <- Server.newEntity
  overworld <- Dimension.new @Dimension.Overworld @384 chunkloading (fromString $ configRootPath cfg <> "/world/region") overworldEid
  -- set this world as the overworld default
  Server.set (coerce $ Server.getComponentId @Dimension.Overworld) overworld

  --nether <- Server.newEntity
  --Dimension.new @Dimension.Overworld "./server/world/region" nether

  --theEnd <- Server.newEntity
  --Dimension.new @Dimension.TheEnd "./server/world/region" theEnd

  -- start network and then gameloop
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
            print e
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
      context @Verbose "tick" $ do
        start <- liftBase $ fromIntegral . Chronos.getTime <$> Chronos.now

        -- Sync all async changes to the world state
        Server.sync

        -- join players
        System.joinPlayers

        -- process all the stuff the clients sent
        System.networkCommands

        -- player block placing/breaking

        -- weather
        -- => iterate all things that have weather components
        -- => What do we do for redstone or other listeners?

        -- create entities

        -- world time
        -- tile events
        -- light updates
        -- create block entities
        -- weather events
        -- random events

        -- player movement -- collision?
        System.playerMovement

        -- update villages
        -- block events
        -- update entities
        -- update block entities

        -- TODO Figure out where this belongs
        -- Apply chunk updates 

        System.removePlayers

        end <- liftBase $ fromIntegral . Chronos.getTime <$> Chronos.now
        let diff = (min 0 $ end - start) `div` 1000
            tickDelay = 1000000 `div` 20 -- 20 ticks a second in microseconds
        liftBase . when (diff > tickDelay `div` 5) $ putStrLn $ "Exceeded tick delay " <> show diff
        liftBase . threadDelay $ tickDelay - diff
      go
