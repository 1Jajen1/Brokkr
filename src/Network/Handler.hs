module Network.Handler (
  handleConnection
) where

import Command

import Control.Concurrent (threadDelay)

import Control.Exception (bracket_)

import qualified Chronos

import qualified Control.Concurrent.Async as Async

import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as LBS

import Data.Text (Text)

import Data.UUID

import qualified Data.Vector as V

import qualified Dimension

import qualified Entity.EntityId as EntityId

import Network.Connection (Connection)
import qualified Network.Connection as Conn

import Network.Monad
import Network.Protocol

import qualified Network.Packet.Client.Login as Client.Login

import qualified Network.Packet.Server.Handshake as Server.Handshake
import qualified Network.Packet.Server.Login as Server.Login
import qualified Network.Packet.Server.Play as Server.Play

import Network.Util.Builder

import qualified Client hiding (new)
import qualified Client.Internal as Client

import Server (Server)
import qualified Server

import Util.Binary

--
import Debug.Trace

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
    Server.Login.LoginStart uName _ _ -> pure uName
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
handlePlay playerUUID playerName finalProtocol = do
  server <- getServer
  sock <- getSocket
  conn <- liftIO Conn.new

  let send = go
        where
        go = do
          Conn.flushPackets conn $ \ps -> do
            -- TODO Ugly
            runNetwork server sock . sendBytes . LBS.fromChunks
              $ fmap (\(Conn.SendPacket szHint packet) -> let !bs = toStrictSizePrefixedByteString finalProtocol szHint $ put packet in bs) (traceShowId $ V.toList ps)
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

  withAsync send $ \sendAs ->
    withAsync keepAlive $ \keepAliveAs -> do
      -- If any of the linked threads crash, we must also crash since the connection will no longer work
      liftIO $ Async.link sendAs
      liftIO $ Async.link keepAliveAs

      -- Create/Load player data
      eid <- liftIO . EntityId.allocateEntityId $ Server.freshEntityId server
      let dim = Dimension.getDimension (Server.dimensions server) Dimension.Overworld
      player <- liftIO $ Client.new conn dim eid playerUUID

      -- Enqueue a player join command      
      liftIO $ Conn.executeCommand conn JoinClient

      liftIO $ bracket_
        (Client.addClient (Server.connectedClients server) player)
        (liftIO $ Conn.executeCommand conn DisconnectClient)
        . runNetwork server sock $ do
    
        -- packet loop
        let handlePlayPackets = do
              readPacket finalProtocol >>= handlePacket server conn
              handlePlayPackets

        _ <- handlePlayPackets
      
        -- The packet loop already diverges, but if for whatever reason we end up here, just kill the connection
        closeConnection

-- Handle play packets
handlePacket :: Server -> Connection -> Server.Play.Packet -> Network ()

handlePacket _ conn (Server.Play.KeepAlive res) = liftIO $ Conn.ackKeepAlive conn res

handlePacket _ conn (Server.Play.SetPlayerPositionAndRotation pos rot onGround) =
  liftIO . Conn.executeCommand conn $ MoveAndRotateClient pos rot onGround
handlePacket _ conn (Server.Play.SetPlayerPosition pos onGround) =
  liftIO . Conn.executeCommand conn $ MoveClient pos onGround
handlePacket _ conn (Server.Play.SetPlayerRotation rot onGround) =
  liftIO . Conn.executeCommand conn $ RotateClient rot onGround
handlePacket _ conn (Server.Play.SetPlayerOnGround onGround) =
  liftIO . Conn.executeCommand conn $ SetOnGround onGround

handlePacket _ _ p = trace ("Unhandled packet: " <> show p) $ pure ()
