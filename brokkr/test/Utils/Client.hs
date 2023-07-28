{-# LANGUAGE RecordWildCards, DeriveAnyClass #-}
module Utils.Client (
  TestServer(..)
, setupTestServer
, stopTestServer
-- client
, TestClient(..)
-- client setup
, newTestClient
, stopTestClient
, readPacket
, sendPacket
, TimeoutException(..)
, TestDimSize
) where

import Brokkr.Packet.Common (Position(..))
import Brokkr.Packet.Decode qualified as Decode
import Brokkr.Packet.Encode qualified as Encode

import Control.Concurrent.Async

import Network.Simple.TCP qualified as Network

import Server
import Server.Config

-- TODO Sort
import Control.Exception (Exception, SomeException(..), throwIO)
import Control.Exception.Base qualified as BE 
import Control.Exception.Safe qualified as SE
import Control.Monad.Fix
import Network.Exception
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.IORef
import Control.Monad
import System.Timeout (timeout)
import Data.Text
import GHC.Conc (labelThread, myThreadId)
import Data.Word
import Server.Monad (Universe)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy

type TestDimSize = 384

-- A normal server with a config suitable for testing (low render distance and other perf options)
data TestServer = TestServer {
  serverThread :: Async ()
, serverHost :: !Network.HostName
, serverPort :: !Network.ServiceName
, serverUniverse :: !Universe
, serverFreeAreasRef :: MVar (NonEmpty Position)
}

setupTestServer :: IO TestServer
setupTestServer = do
  universeRef <- newEmptyMVar
  serverReadyRef <- newEmptyMVar
  as <- async $ do
    myThreadId >>= flip labelThread "Test server"
    newWorld >>= \u -> do
      putMVar universeRef u
      runServerM testConfig u . setupServer $ putMVar serverReadyRef ()
  link as -- Crash the test suite if the server crashes

  let maxServerReadyTime = 10_000
  timeout maxServerReadyTime (takeMVar serverReadyRef) >>= \case
    Nothing -> throwIO ServerNotReadyInTime
    Just _ -> pure ()

  serverUniverse <- takeMVar universeRef

  let worldHeight = -60 -- TODO Height for superflat world
      areaSize = 2
      toCoord x = x * 16 * areaSize + 16 * (areaSize / 2)
      go n = [(x,n) | x <- [0..n - 1]] <> [(n, x) | x <- [0..n - 1]] <> [(n,n)] <> go (n + 1)
      generateFreeAreas = do
        (x,z) <- (1,1) :| go 2
        pure $ Position (toCoord x) worldHeight (toCoord z)

  serverFreeAreasRef <- newMVar generateFreeAreas

  pure $ TestServer as testHost port serverUniverse serverFreeAreasRef
  where
    port = "25565" -- TODO from args or random
    testHost = "127.0.0.1"
    testConfig = Config {
        configHostPreference = Network.Host testHost
      , configServiceName    = port
      , configChunkloadingThreads = 4
      , configServerRenderDistance = 2
      , configRootPath = "./test"
      -- TODO Get path from work dir and also make the server a test world instead
      }

data ServerNotReadyInTime = ServerNotReadyInTime
  deriving stock Show
  deriving anyclass Exception

stopTestServer :: TestServer -> IO ()
stopTestServer = cancel . serverThread

-- A client that records everything and is connected to a test server
-- - Records all in/out bound traffic in uncompressed and unencrypted format
-- - Can install hooks on the incoming packets
--   - Packets can be tested in order or out of order. E.g. the login sequence should
--     always arrive in order, but once connected any other packet could come in between
data TestClient = TestClient {
  clientThread :: Async ()
, clientTestServer :: !TestServer
, clientReqRef :: MVar ClientReq
, clientName :: !Text
, clientProtocolVersion :: !Int
, clientAddress :: !Text
, clientPort :: !Word16
-- clientEntityId :: EntityId -- TODO Get from join game packet in joinGame, allows us to query the server
}

newTestClient :: Text -> TestServer -> IO TestClient
newTestClient clientName clientTestServer@TestServer{serverHost, serverPort} = do
  let maxClientReadyTime = 20_000
  clientReadyRef <- newEmptyMVar

  let clientProtocolVersion = 1
      clientAddress = "local"
      clientPort = 1

  clientReqRef <- newEmptyMVar
  clientThread <- async $ do
    myThreadId >>= flip labelThread "Test client"
    -- handle (\(e :: SomeException) -> putStrLn "Client exception" >> print e) $
    do
      Network.connect serverHost serverPort $ \(sock,_) -> do
        putMVar clientReadyRef ()
        stateRef <- newIORef $ ClientNetworkState mempty Decode.NoCompression Decode.NoEncryption
        -- The client is in a loop taking requests for either reading or writing
        void . fix $ \go -> do
          -- Catch timeouts so that we can cancel io but still continue working with the client
          -- Specifically only catch timeouts, cancellations from async or other errors should still crash us
          SE.handleAsync (\(e :: TimeoutException) -> print ()) $ do
            takeMVar clientReqRef >>= \case
              ReadReq onErr f -> SE.handle onErr $ do
                ClientNetworkState bs cs es <- readIORef stateRef
                (a, bs') <- Decode.readPacket
                  (Proxy @Decode.SomeCompression) cs es
                  (Network.recv sock 1024 >>= \case Nothing -> throwIO ConnectionClosed; Just x -> pure x)
                  bs $ \a bs -> pure (a, bs)
                (cs', es') <- f cs es a
                writeIORef stateRef $ ClientNetworkState bs' cs' es'
              WriteReq a onErr onWrite -> SE.handle onErr $ do
                ClientNetworkState _ cs es <- readIORef stateRef
                let bs = Encode.toStrictByteString (Proxy @Decode.SomeCompression) cs es a
                Network.send sock bs
                onWrite
          go
  timeout maxClientReadyTime (takeMVar clientReadyRef) >>= \case
    Nothing -> throwIO ClientNotReadyInTime
    Just _ -> pure ()
  -- link clientThread -- TODO
  pure TestClient{..}

data ClientNotReadyInTime = ClientNotReadyInTime
  deriving stock Show
  deriving anyclass Exception

stopTestClient :: TestClient -> IO ()
stopTestClient = cancel . clientThread

data ClientNetworkState = ClientNetworkState !ByteString !Decode.CompressionSettings !Decode.EncryptionSettings

data ClientReq where
  ReadReq  :: (Show a, Decode.FromBinary a)
    => (SomeException -> IO ())
    -> (Decode.CompressionSettings -> Decode.EncryptionSettings -> a -> IO (Decode.CompressionSettings, Decode.EncryptionSettings))
    -> ClientReq
  WriteReq :: Encode.ToBinary a => Encode.Packet a -> (SomeException -> IO ()) -> IO () -> ClientReq

readPacket :: (Show a, Decode.FromBinary a)
  => Int -> TestClient
  -> (Decode.CompressionSettings -> Decode.EncryptionSettings -> a -> (Decode.CompressionSettings, Decode.EncryptionSettings))
  -> IO (Either SomeException a)
readPacket maxTime TestClient{clientReqRef, clientThread} changeProtocol = do
  resVar <- newEmptyMVar
  putMVar clientReqRef $ ReadReq (putMVar resVar . Left) $ \cs es a -> do
    let newProt = changeProtocol cs es a
    putMVar resVar $ Right a
    pure newProt
  timeout maxTime (takeMVar resVar) >>= \case
    Nothing -> do
      -- We want to cancel the io op, but not the whole req-loop, so throw a timeout exception
      -- which we will catch and ignore
      BE.throwTo (asyncThreadId clientThread) TimeoutException
      pure $ Left (SomeException TimeoutException)
    Just x  -> pure x

data TimeoutException = TimeoutException
  deriving stock Show
  deriving anyclass Exception

sendPacket :: Encode.ToBinary a => Int -> TestClient -> Encode.Packet a -> IO (Maybe SomeException)
sendPacket maxTime TestClient{clientReqRef,clientThread} a = do
  resVar <- newEmptyMVar
  putMVar clientReqRef $ WriteReq a (putMVar resVar . Just) (putMVar resVar Nothing)
  timeout maxTime (takeMVar resVar) >>= \case
    Nothing -> do
      -- We want to cancel the io op, but not the whole req-loop, so throw a timeout exception
      -- which we will catch and ignore
      BE.throwTo (asyncThreadId clientThread) TimeoutException
      pure $ Just (SomeException TimeoutException)
    Just x -> pure x
