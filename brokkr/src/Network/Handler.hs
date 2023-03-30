module Network.Handler (
  handleConnection
) where

import Data.IORef
import Chunk.Position
import IO.Chunkloading
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception
import qualified Chronos
import qualified Control.Concurrent.Async as Async
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Util.UUID
import qualified Data.Vector as V
import Dimension (Dimension, DimensionType(Overworld))
import qualified Dimension
import Network.Connection (Connection, Command(..))
import qualified Network.Connection as Conn
import Network.Monad
import Network.Protocol
import qualified Network.Packet.Client.Login as Client.Login
import qualified Network.Packet.Client.Play as Client.Play
import qualified Network.Packet.Server.Handshake as Server.Handshake
import qualified Network.Packet.Server.Login as Server.Login
import qualified Network.Packet.Server.Play as Server.Play
import Network.Packet.Client.Play.ChunkData (mkChunkData)
import Network.Util.Builder
import Util.Binary
import Data.Coerce
import Control.Monad.Trans.Control
import Util.Position
import Util.Rotation
import qualified Data.Text as T
import qualified Network.Packet.Client.Play as C
import qualified Network.Packet.Client.Play.Login as C 
import Brokkr.Registry.Biome
import Brokkr.Registry.Dimension
import Client.GameMode
import Block.Position
import qualified Data.Bitfield as Bitfield

import qualified Server as Hecs
import qualified Hecs
import Hecs.Entity.Internal (Entity(..))

--

handleConnection :: Network ()
handleConnection = do
  LoginRes uid uName prot <- readPacket (Protocol NoCompression NoEncryption) >>= \case
    Server.Handshake.Handshake _ _ _ Server.Handshake.Status -> handleStatus
    Server.Handshake.Handshake _ _ _ Server.Handshake.Login  -> handleLogin
  handlePlay uid uName prot

handleStatus :: Network a
handleStatus = closeConnection

handleLogin :: Network LoginResult
handleLogin = do
  uName <- readPacket (Protocol NoCompression NoEncryption) >>= \case
    Server.Login.LoginStart uName _ -> pure uName
    -- TODO Check for other packets and throw InvalidProtocol on those

  -- TODO generate proper uid
  let uid = nil

  -- TODO Have a config check if we use compression and what the threshold is
  let thresh = 512
  sendPackets (Protocol NoCompression NoEncryption) 10 [Client.Login.SetCompression thresh]

  let finalProtocol = Protocol (Threshold thresh) NoEncryption
  sendPackets finalProtocol 64 [Client.Login.LoginSuccess uid uName]

  pure $ LoginRes uid uName finalProtocol

data LoginResult = LoginRes UUID Text Protocol

handlePlay :: UUID -> Text -> Protocol -> Network a
handlePlay _playerUUID _playerName finalProtocol = do
  !conn <- liftIO Conn.new

  let send = go
        where
          go = do
            _ <- liftBaseWith $ \runInBase -> Conn.flushPackets conn $ \ps -> do
              runInBase . sendBytes . LBS.fromChunks
                $! fmap (\(Conn.SendPacket szHint packet) -> let !bs = toStrictSizePrefixedByteString finalProtocol szHint $ put packet in bs) $ V.toList ps
            go
      keepAlive = go
        where
          -- TODO make a config option so testing is a little easier 
          delay = fromIntegral $ ((Chronos.getTimespan Chronos.second) * 20) `div` 1000
          go = do
            threadDelay delay
            t' <- Chronos.getTime <$> Chronos.now
            Conn.sendKeepAlive conn t'
            go

  _ <- liftBaseWith $ \runInBase ->
    -- TODO Label these threads
    Async.withAsync (runInBase send) $ \sendAs ->
      Async.withAsync keepAlive $ \keepAliveAs -> do
        -- If any of the linked threads crash, we must also crash since the connection will no longer work
        Async.link sendAs
        Async.link keepAliveAs

        -- Actually create the player entity, from that point on the connection is visible globally
        bracket
          (do
            bracketOnError
              (runInBase $ Hecs.newEntity)
              (\eidSt -> runInBase $ restoreM eidSt >>= Hecs.freeEntity) -- TODO Log
              (\eidSt -> runInBase $ restoreM eidSt >>= \eid -> joinPlayer conn eid >> pure eid)
          ) (\_ -> putStrLn "Closed") -- TODO 
            $ \clientSt -> runInBase $ restoreM clientSt >>= \_client -> do
              -- packet loop
              let handlePlayPackets = do
                    readPacket finalProtocol >>= handlePacket conn
                    handlePlayPackets

              handlePlayPackets

  -- The packet loop in the bracket already diverges, but if for whatever reason we end up here, just kill the connection
  closeConnection

joinPlayer :: Connection -> Hecs.EntityId -> Network ()
joinPlayer conn eid = do
  -- Create/Load player data -- SYNC Because we need to know where to join the player
  -- TODO Move
  let initialPosition = Position 0 150 0
      initialRotation = Rotation 0 0
      initialVelocity = Position 0 0 0
  
  initialWorld <- Hecs.get @Dimension (coerce $ Hecs.getComponentId @Dimension.Overworld)
    pure
    (error "World singleton missing")
  let viewDistance = 32 -- Sync with PlayerMovement until I store it as a component
  let loginData = C.LoginData
        (fromIntegral . Bitfield.get @"eid" $ Hecs.unEntityId eid)
        (C.IsHardcore False)
        Creative
        C.Undefined
        (V.fromList ["minecraft:overworld", "minecraft:nether", "minecraft:the_end"])
        (C.RegistryCodec dimRegistry biomeRegistry chatRegistry)
        "minecraft:overworld" -- TODO proper type
        "minecraft:overworld"
        (C.HashedSeed 0)
        (C.MaxPlayers 5)
        (C.ViewDistance viewDistance)
        (C.SimulationDistance 8)
        (C.ReducedDebugInfo False)
        (C.EnableRespawnScreen False)
        (C.IsDebug False)
        (C.IsFlat False)
        C.NoDeathLocation
      dimRegistry = C.DimensionTypeRegistry
        "minecraft:dimension_type"
        (V.fromList
          [
            C.DimensionRegistryEntry "minecraft:overworld" 0 overworld
          , C.DimensionRegistryEntry "minecraft:nether" 1 nether
          , C.DimensionRegistryEntry "minecraft:the_end" 2 end
          ]
        )
      biomeRegistry = C.BiomeRegistry "minecraft:worldgen/biome" . V.fromList $ do
        (bid, (name, settings)) <- zip [0..] all_biome_settings
        return $ C.BiomeRegistryEntry ("minecraft:" <> T.pack name) bid settings
      chatRegistry = C.ChatRegistry "minecraft:chat_type" mempty

  liftIO . Conn.sendPacket conn $ Conn.SendPacket 65536 (C.Login loginData)
  liftIO . Conn.sendPacket conn $ Conn.SendPacket 10 (C.SetDefaultSpawnPosition (BlockPos 0 130 0) 0)

  -- Preload chunks for this player -- SYNC Because we don't want to wait for this on the main thread and this will join the
                                    --  player with chunks loaded and not in some void
  chunkloading <- Hecs.getSingleton @Chunkloading
  let chunksToLoad = [ChunkPos x z | x <- [-viewDistance..viewDistance], z <- [-viewDistance..viewDistance] ]
      numChunksToLoad = (viewDistance * 2 + 1) * (viewDistance * 2 + 1)
  rPath <- Hecs.get @Dimension.RegionFilePath (Dimension.entityId initialWorld) pure (error "TODO")

  doneRef <- liftIO $ newEmptyMVar
  numDoneRef <- liftIO $ newIORef numChunksToLoad

  liftIO $ putStrLn $ "Loading " <> show numChunksToLoad <> " chunks"
  --liftIO $ print chunksToLoad

  liftIO $ loadChunks (coerce rPath) chunksToLoad chunkloading $ \mvar -> void . Async.async $ takeMVar mvar >>= \c -> do
    let !(!sz, !cData) = mkChunkData c
    Conn.sendPacket conn . Conn.SendPacket sz $! Client.Play.ChunkDataAndUpdateLight cData
    done <- atomicModifyIORef' numDoneRef $ \(i :: Int) -> (i - 1, i - 1)
    when (done == 0) $ putMVar doneRef ()
  
  liftIO $ takeMVar doneRef
  liftIO $ putStrLn "Done sending chunks"
  
  liftIO . Conn.sendPacket conn $ Conn.SendPacket 10 (C.SetCenterChunk 0 0)
  liftIO . Conn.sendPacket conn $ Conn.SendPacket 64 (C.SynchronizePlayerPosition (Position 0 130 0) (Rotation 180 0) (C.TeleportId 0) C.NoDismount)

  Hecs.set eid initialPosition
  Hecs.set eid initialRotation
  Hecs.set eid initialVelocity
  Hecs.set eid initialWorld
  
  -- This is the write that exposes us to the global state proper because that is what the JoinPlayer system queries on
  Hecs.set eid conn

-- Handle play packets
handlePacket :: Connection -> Server.Play.Packet -> Network ()

handlePacket conn (Server.Play.KeepAlive res) = liftIO $ Conn.ackKeepAlive conn res

handlePacket conn (Server.Play.SetPlayerPositionAndRotation pos rot onGround) =
  liftIO . Conn.pushCommand conn $ MoveAndRotateClient pos rot onGround
handlePacket conn (Server.Play.SetPlayerPosition pos onGround) =
  liftIO . Conn.pushCommand conn $ MoveClient pos onGround
handlePacket conn (Server.Play.SetPlayerRotation rot onGround) =
  liftIO . Conn.pushCommand conn $ RotateClient rot onGround
handlePacket conn (Server.Play.SetPlayerOnGround onGround) =
  liftIO . Conn.pushCommand conn $ SetOnGround onGround

handlePacket _ p = liftIO $ putStrLn ("Unhandled packet: " <> show p)
