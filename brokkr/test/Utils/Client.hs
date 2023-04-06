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
) where

import Control.Concurrent.Async

import Network.Monad (readPacket')
import Network.Simple.TCP qualified as Network

import Server
import Server.Config

-- TODO Sort
import Control.Exception.Safe
import Control.Monad.Fix
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Network.Protocol
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.IORef
import Control.Monad
import Network.Util.Builder
import Util.Binary as Binary
import System.Timeout (timeout)
import Data.Text
import GHC.Conc (labelThread, myThreadId)
import Data.Word
import Server.Monad (Universe)
import Util.Position
import Data.List.NonEmpty (NonEmpty(..))

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
}

newTestClient :: Text -> TestServer -> IO TestClient
newTestClient clientName clientTestServer@TestServer{serverHost, serverPort} = do
  let maxClientReadyTime = 10_000
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
        stateRef <- newIORef $ ClientNetworkState mempty (Protocol NoCompression NoEncryption)
        -- The client is in a loop taking requests for either reading or writing
        void . fix $ \go -> do
          takeMVar clientReqRef >>= \case
            ReadReq onErr f -> handle onErr $ do
              ClientNetworkState bs prot <- readIORef stateRef
              (a, bs') <- flip runStateT bs . flip runReaderT sock $ readPacket' prot
              prot' <- f prot a
              writeIORef stateRef $ ClientNetworkState bs' prot'
            WriteReq szEstimate a onErr onWrite -> handle onErr $ do
              ClientNetworkState _ prot <- readIORef stateRef
              let bs = toStrictSizePrefixedByteString prot szEstimate $ Binary.put a
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

data ClientNetworkState = ClientNetworkState !ByteString !Protocol

data ClientReq where
  ReadReq  :: (Show a, FromBinary a) => (SomeException -> IO ()) -> (Protocol -> a -> IO Protocol) -> ClientReq
  WriteReq :: ToBinary a => Int -> a -> (SomeException -> IO ()) -> IO () -> ClientReq

readPacket :: (Show a, FromBinary a) => Int -> TestClient -> (Protocol -> a -> Protocol) -> IO (Either SomeException a)
readPacket maxTime TestClient{clientReqRef} changeProtocol = do
  resVar <- newEmptyMVar
  putMVar clientReqRef $ ReadReq (putMVar resVar . Left) $ \prot a -> do
    let newProt = changeProtocol prot a
    putMVar resVar $ Right a
    pure newProt
  timeout maxTime (takeMVar resVar) >>= \case
    Nothing -> pure $ Left (SomeException TimeoutException)
    Just x  -> pure x

data TimeoutException = TimeoutException
  deriving stock Show
  deriving anyclass Exception

sendPacket :: ToBinary a => Int -> TestClient -> Int -> a -> IO (Maybe SomeException)
sendPacket maxTime TestClient{clientReqRef} sz a = do
  resVar <- newEmptyMVar
  putMVar clientReqRef $ WriteReq sz a (putMVar resVar . Just) (putMVar resVar Nothing)
  timeout maxTime (takeMVar resVar) >>= \case
    Nothing -> pure $ Just (SomeException TimeoutException)
    Just x -> pure x
