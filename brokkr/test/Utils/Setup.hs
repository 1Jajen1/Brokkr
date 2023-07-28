{-# LANGUAGE DeriveAnyClass #-}
module Utils.Setup (
  BrokkrSpec
, withBrokkrServerSpec
, withTestClient
, withTestClient_
, TestClient
, sendPacket_
, readPacketWithProtocol_
, readPacketOrTimeout_
, readPacket_
, unexpectedPacket
) where

import Brokkr.Packet.Binary
import Brokkr.Packet.Encode qualified as Encode

import Test.Syd
import Control.Exception

import Utils.Client

-- TODO
import Data.Text
import Control.Monad.Trans.Resource
import Data.Typeable

{-

This file for convenience functions around creating and destroying Testservers and Testclients

Utils/Client has all the functionality for doing stuff with those

-- TODO List:

- Create a superflat test world and pregenerate a large area
- Setup global state that assigns random isolated areas of that
  world to clients when they request it
  - Regions come in different sizes for different tests
    - elytra flying needs more space than smaller movement tests
    - placing and breaking blocks also needs very little space
- Assign part of the world to regression tests

- Add tickwarp as a day 1 feature, it is invaluable for tests
  - Testing day-night cycles, random ticks etc all need tick-warping for
    reasonable execution time

-}

type BrokkrSpec = TestDef '[TestServer] ()

withBrokkrServerSpec :: BrokkrSpec -> Spec
withBrokkrServerSpec = setupAroundAll $ SetupFunc $ bracket setupTestServer stopTestServer

withTestClient :: Text -> TestDef '[TestServer] TestClient -> BrokkrSpec
withTestClient name = aroundWith' (\f server _ -> bracket (newTestClient name server) stopTestClient $ \client -> f server client)

withTestClient_ :: MonadResource m => Text -> TestServer -> m TestClient
withTestClient_ name server = snd <$> allocate (newTestClient name server) stopTestClient

sendPacket_ :: ToBinary a => String -> Int -> TestClient -> Encode.Packet a -> IO ()
sendPacket_ name maxTime client toSend = sendPacket maxTime client toSend >>= \case
  Nothing -> pure ()
  Just e -> throwIO $ ExceptionOnSend name e

data ExceptionOnSend = ExceptionOnSend String SomeException
  deriving stock Show
  deriving anyclass Exception

readPacketWithProtocol_ :: (Show a, FromBinary a)
  => String -> Int -> TestClient
  -> (Encode.CompressionSettings -> Encode.EncryptionSettings -> a -> (Encode.CompressionSettings, Encode.EncryptionSettings))
  -> (a -> IO r) -> IO r
readPacketWithProtocol_ name maxTime client changeProt f = readPacket maxTime client changeProt >>= \case
  Left e -> throwIO $ ExceptionOnRead name e
  Right a -> f a

readPacket_ :: (Show a, FromBinary a) => String -> Int -> TestClient -> (a -> IO r) -> IO r
readPacket_ name maxTime client f = readPacket maxTime client (\cs es _ -> (cs, es)) >>= \case
  Left e -> throwIO $ ExceptionOnRead name e
  Right a -> f a

readPacketOrTimeout_ :: (Show a, FromBinary a) => String -> Int -> TestClient -> (Maybe a -> IO r) -> IO r
readPacketOrTimeout_ name maxTime client f = readPacket maxTime client (\cs es _ -> (cs, es)) >>= \case
  Left (SomeException e) | typeOf e == typeRep @_ @TimeoutException undefined -> f Nothing
  Left e -> throwIO $ ExceptionOnRead name e
  Right a -> f $ Just a

data ExceptionOnRead = ExceptionOnRead String SomeException
  deriving stock Show
  deriving anyclass Exception

unexpectedPacket :: Show a => String -> a -> IO ()
unexpectedPacket name a = throwIO $ UnexpectedPacket name (show a)

data UnexpectedPacket = UnexpectedPacket String String
  deriving stock Show
  deriving anyclass Exception
