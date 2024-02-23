module Brokkr.Network.Handler (
  handleConnection
) where

import Brokkr.Client.Username

import Brokkr.Chunk.Internal (force)
import Brokkr.Chunk.Position

import Brokkr.Debug.Monad

import Brokkr.Dimension (Dimension)
import Brokkr.Dimension qualified as Dimension

import Brokkr.IO.ChunkCache (ChunkTicket(ChunkTicket))

import Brokkr.Network.Connection (Connection)
import Brokkr.Network.Connection qualified as Conn

import Brokkr.Network.Monad
import Brokkr.Network.Util.Chunk

import Brokkr.Packet.Settings
import Brokkr.Packet.Encode qualified as Encode

import Brokkr.Packet.ServerToClient.Login qualified as SC.Login
import Brokkr.Packet.ServerToClient.Play qualified as SC.Play
-- TODO Offer TH based smart constructor for literals and remove this import
import Brokkr.Packet.ServerToClient.Play.Types.Internal (DimensionName(..), BiomeName(..), DimensionType(..))
import Brokkr.Packet.ClientToServer.Handshake qualified as CS.Handshake
import Brokkr.Packet.ClientToServer.Login qualified as CS.Login
import Brokkr.Packet.ClientToServer.Play qualified as CS.Play

import Brokkr.Registry.Biome
import Brokkr.Registry.Dimension

import Brokkr.Server.Config
import Brokkr.Server.Monad qualified as Server
import Brokkr.System.JoinPlayers (JoinPlayer, RemovePlayer)
import Brokkr.System.PlayerMovement (ChunkYPosition(..))

import Brokkr.Util.Position
import Brokkr.Util.Rotation

import Chronos qualified

import Control.Concurrent (threadDelay, myThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.STM

import Control.Concurrent.Async qualified as Async

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import Control.Exception

import Data.Bitfield qualified as Bitfield

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Data.Coerce
import Data.IORef

import Data.Proxy

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import Data.UUID
import Data.UUID.V3 qualified as UUID.V3 

import Data.Vector qualified as V

import GHC.Conc (labelThread)

import Hecs.Entity.Internal as Server (unEntityId, Entity(..))


handleConnection :: Network ()
handleConnection = context @Info "Network" $ do
  LoginRes uid uName cs es <- readPacket (Proxy @NoCompression) NoCompression NoEncryption >>= \case
    CS.Handshake.Handshake _ _ _ CS.Handshake.Status -> handleStatus
    CS.Handshake.Handshake _ _ _ CS.Handshake.Login  -> handleLogin
  context @Info (show $ coerce @_ @UUID uid) $ do
    debug @Info $ show (CS.Play.unUsername uName) <> " logged in" 
    handlePlay (coerce uid) (coerce . CS.Play.unUsername $ uName) cs es

handleStatus :: Network a
handleStatus = closeConnection

handleLogin :: Network LoginResult
handleLogin = do
  uName <- readPacket (Proxy @NoCompression) NoCompression NoEncryption >>= \case
    CS.Login.LoginStart uName _ -> pure uName
    -- TODO Check for other packets and throw InvalidProtocol on those

  let offlineNamespace = UUID.V3.generateNamed nil (BS.unpack "OfflinePlayer")
      uid = CS.Play.UserUUID $ UUID.V3.generateNamed offlineNamespace (BS.unpack $ T.encodeUtf8 $ CS.Play.unUsername uName)

  -- TODO Have a config check if we use compression and what the threshold is
  let thresh = 512000 -- TODO Compression is broken right now
  sendPackets (Proxy @NoCompression) NoCompression NoEncryption [Encode.Packet (Conn.EstimateMin 10) $ SC.Login.SetCompression $ SC.Login.CompressionThreshold thresh]
  
  let finalCs = UseCompression (fromIntegral thresh)
      finalEs = NoEncryption
  
  sendPackets (Proxy @SomeCompression) finalCs finalEs [Encode.Packet (Conn.EstimateMin 64) $ SC.Login.LoginSuccess uid uName mempty]

  pure $ LoginRes uid uName finalCs finalEs

data LoginResult = LoginRes !CS.Play.UserUUID !CS.Play.Username !CompressionSettings !EncryptionSettings

handlePlay :: ClientUUID -> Username -> CompressionSettings -> EncryptionSettings -> Network a
handlePlay playerUUID playerName cs es = do
  !conn <- liftIO Conn.new

  liftIO $ myThreadId >>= flip labelThread ("Network thread for: " <> T.unpack (coerce playerName))

  comp <- getCompressor

-- TODO another keepalive thread that checks if the client answered in time!
  let send = do
        liftIO $ myThreadId >>= flip labelThread ("Network send for: " <> T.unpack (coerce playerName))
        go
        where
          go = do
            _ <- liftBaseWith $ \runInBase -> Conn.flushPackets conn $ \ps -> do
              bs <- evaluate . LBS.fromChunks . fmap (\(Conn.SendPacket p) -> let !bs = Encode.toStrictByteString (Proxy @SomeCompression) comp cs es p in bs) $ V.toList ps
              runInBase $ sendBytes bs
              -- TODO Is this enough strictness? Does this even work?!
            go
      keepAlive = do
        liftIO $ myThreadId >>= flip labelThread ("Network keep-alive for: " <> T.unpack (coerce playerName))
        go
        where
          -- TODO make a config option so testing is a little easier 
          delay = fromIntegral $ (Chronos.getTimespan Chronos.second * 20) `div` 1000
          go = do
            threadDelay delay
            t' <- Chronos.getTime <$> Chronos.now
            Conn.sendKeepAlive conn t'
            go

  _ <- liftBaseWith $ \runInBase ->
    -- TODO Label these threads
    Async.withAsync (runInBase send) $ \sendAs ->
      Async.withAsync keepAlive $ \keepAliveAs -> do
        -- If any of the send threads crash, we must also crash since the connection will no longer work
        Async.link sendAs
        Async.link keepAliveAs

        -- Actually create the player entity, from that point on the connection is visible globally
        bracket
          (do
            bracketOnError
              (runInBase Server.newEntity)
              (\eidSt -> runInBase $ do
                restoreM eidSt >>= Server.freeEntity
                debug @Error "Failed to join player")
              (\eidSt -> runInBase $ restoreM eidSt >>= \eid -> joinPlayer conn playerName playerUUID eid >> pure eid)
          ) (\(eid, _) -> void . runInBase $ Server.addTag @RemovePlayer eid)
            $ \clientSt -> runInBase $ restoreM clientSt >>= \_client -> do
              debug @Debug "Player joined"
              -- packet loop
              let handlePlayPackets = do
                    readPacket (Proxy @SomeCompression) cs es >>= \case
                      CS.Play.KeepAlive t -> liftIO $ Conn.ackKeepAlive conn t
                      p -> liftIO $ Conn.pushCommand conn p
                    handlePlayPackets

              handlePlayPackets

  -- The packet loop in the bracket already diverges, but if for whatever reason we end up here, just kill the connection
  closeConnection

joinPlayer :: Connection -> Username -> ClientUUID -> Server.EntityId -> Network ()
joinPlayer conn username clientUUID eid = do
  -- Create/Load player data -- SYNC Because we need to know where to join the player
  -- TODO Move
  let initialYPosition = 150
      initialPosition = Position 0 initialYPosition 0
      initialRotation = Rotation 0 0
      initialVelocity = Position 0 0 0
      initialChunkYPosition = ChunkYPosition $ floor initialYPosition `div` 16

  initialWorld <- Server.get @Dimension (coerce $ Server.getComponentId @Dimension.Overworld) pure
    $ error "World singleton missing" -- TODO error. Also check other cases similar to this

  -- TODO ugly
  viewDistance <- configServerRenderDistance <$> getConfig

  let loginCodec = SC.Play.RegistryCodec dimRegistry biomeRegistry chatRegistry
      dimRegistry = SC.Play.mkDimensionRegistry $ V.fromList [
          (UnsafeDimensionName "minecraft:overworld", overworld)
        , (UnsafeDimensionName "minecraft:nether",    nether   )
        , (UnsafeDimensionName "minecraft:end",       end      )
        ]
      -- biomeRegistry = SC.Play.mkBiomeRegistry mempty
      biomeRegistry = SC.Play.mkBiomeRegistry $ V.fromList $ do
        (name, settings) <- all_biome_settings
        return (UnsafeBiomeName $ "minecraft:" <> T.pack name, settings)
      chatRegistry = SC.Play.mkChatRegistry mempty

  -- TODO Accurate size
  liftIO . Conn.sendPacket @Conn.UnsafeDefDimHeight conn . Conn.Packet (Conn.EstimateMin 16384) $ SC.Play.Login
    (SC.Play.EntityId . fromIntegral . Bitfield.get @"eid" $ Server.unEntityId eid)
    SC.Play.NotHardcore
    SC.Play.Creative
    SC.Play.PreviousGameModeUndefined
    (V.fromList [coerce @Text "minecraft:overworld", coerce @Text "minecraft:nether", coerce @Text "minecraft:the_end"])
    loginCodec
    (coerce @Text "minecraft:overworld")
    (coerce @Text "minecraft:overworld")
    (SC.Play.HashedSeed 0)
    (SC.Play.MaxPlayers 5)
    (SC.Play.ViewDistance (fromIntegral viewDistance))
    (SC.Play.SimulationDistance 8)
    SC.Play.NoReducedDebugInfo
    SC.Play.EnableRespawnScreen
    SC.Play.IsNotDebug -- This is bad!
    SC.Play.IsFlat
    Nothing

  liftIO . Conn.sendPacket @Conn.UnsafeDefDimHeight conn
    $ Conn.Packet (Conn.EstimateMin 28) (SC.Play.SetDefaultSpawnPosition (SC.Play.Location 0 130 0) (SC.Play.SpawnAngle 0))

  -- Preload chunks for this player -- SYNC Because we don't want to wait for this on the main thread and this will join the
                                    --  player with chunks loaded and not in some void
  let chunksToLoad = [ChunkPosition x z | x <- [-viewDistance..viewDistance], z <- [-viewDistance..viewDistance]]
      numChunksToLoad = (viewDistance * 2 + 1) * (viewDistance * 2 + 1)
      playerTicket = ChunkTicket eid

  doneRef <- liftIO newEmptyMVar
  numDoneRef <- liftIO $ newIORef numChunksToLoad

  -- liftIO $ putStrLn $ "Loading " <> show numChunksToLoad <> " chunks"
  -- liftIO $ print chunksToLoad
  -- debug @Verbose $ "Requesting " <> show numChunksToLoad <> " chunks"

  Dimension.withChunkLoading initialWorld $ \(Proxy :: Proxy dimHeight) loadChunk -> liftIO $ forM_ chunksToLoad $ \cpos -> do
    as <- Async.async $ loadChunk playerTicket cpos >>= \ref -> do
      -- Force all delayed chunk updates
      !c <- atomically $ do
        !c <- force <$> readTVar ref
        writeTVar ref c
        pure c
      let (sz, cData) = mkChunkPacket c -- traceShowId $ mkChunkPacket c
      Conn.sendPacket @dimHeight conn . Conn.Packet (Conn.EstimateMin sz) $! cData
      done <- atomicModifyIORef' numDoneRef $ \(i :: Int) -> (i - 1, i - 1)
      when (done == 0) $ putMVar doneRef ()
    Async.link as

  liftIO $ takeMVar doneRef
  -- debug @Verbose $ "Sent " <> show numChunksToLoad <> " chunks"

  liftIO . Conn.sendPacket @Conn.UnsafeDefDimHeight conn $ Conn.Packet (Conn.EstimateMin 10) (SC.Play.SetCenterChunk 0 0)
  liftIO . Conn.sendPacket @Conn.UnsafeDefDimHeight conn $ Conn.Packet (Conn.EstimateMin 38)
    (SC.Play.SynchronizePlayerPosition (SC.Play.Position 0 130 0) (SC.Play.Rotation 180 0) mempty (SC.Play.TeleportId 0) SC.Play.NoDismount)

  -- TODO All of this needs to be somewhere in Brokkr.Client.*
  Server.set eid username
  Server.set eid clientUUID
  Server.set eid initialChunkYPosition
  Server.set eid initialPosition
  Server.set eid initialRotation
  Server.set eid initialVelocity
  Server.set eid initialWorld
  Server.set eid conn

  -- This write will trigger the server to fully join the new client, so make sure it is done last
  -- TODO How can I, or should I, enforce that the required components have been added
  Server.addTag @JoinPlayer eid
