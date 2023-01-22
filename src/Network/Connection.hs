{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Connection (
  Connection
, new
, SendPacket(..)
, pushCommand, pushCommands
, sendPacket, sendPackets
, flushCommands
, flushPackets
, sendKeepAlive
, ackKeepAlive
, close
, Command(..)
) where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.Int

import qualified Data.Vector as V

import GHC.Generics

import Network.Exception (ConnectionClosed(..), ClientTimedOut(..), InvalidKeepAlive(..))

import Network.Packet.Client.Play (Packet(KeepAlive))

import Util.Queue (Queue)
import qualified Util.Queue as Queue

import Util.Ring (Ring)
import qualified Util.Ring as Ring

import Util.Position
import Util.Rotation

import Hecs

data Connection = Conn {
  queue :: Queue Command 
, sendLock :: MVar ()
, sendQueue :: Ring SendPacket
, lastKeepAlive :: MVar Int64 -- Full means we sent the number, empty means the client responded correctly
, threadId :: ThreadId
}
  deriving stock Generic
  deriving Component via (ViaBox Connection)

data SendPacket = SendPacket !Int !Packet
  deriving stock Show

new :: IO Connection
new = do
  queue <- Queue.new 64
  sendLock <- newMVar ()
  sendQueue <- Ring.newRingBuffer 512
  lastKeepAlive <- newEmptyMVar
  threadId <- myThreadId
  pure Conn{..}

pushCommand :: Connection -> Command -> IO ()
pushCommand Conn{..} c = Queue.push queue c

pushCommands :: Connection -> V.Vector Command -> IO ()
pushCommands Conn{..} cs = Queue.pushN queue cs

sendPacket :: Connection -> SendPacket -> IO ()
sendPacket Conn{..} p = withMVar sendLock . const $ Ring.push sendQueue p

sendPackets :: Connection -> V.Vector SendPacket -> IO ()
sendPackets Conn{..} ps = withMVar sendLock . const $ Ring.pushN sendQueue ps

flushCommands :: Connection -> (V.Vector Command -> IO a) -> IO a
flushCommands Conn{..} f = Queue.flush queue >>= f
{-# INLINE flushCommands #-}

flushPackets :: Connection -> (V.Vector SendPacket -> IO a) -> IO a
flushPackets Conn{..} f = do
  ps <- Ring.peekN sendQueue
  res <- f ps
  Ring.advanceN sendQueue . fromIntegral $ V.length ps
  pure res
{-# INLINE flushPackets #-}

sendKeepAlive :: Connection -> Int64 -> IO ()
sendKeepAlive c@Conn{..} t = do
  res <- tryPutMVar lastKeepAlive t
  -- If the client hasn't yet responded, close the connection
  unless res $ throwIO ClientTimedOut
  sendPacket c . SendPacket 10 $ KeepAlive t

ackKeepAlive :: Connection -> Int64 -> IO ()
ackKeepAlive Conn{..} t = tryTakeMVar lastKeepAlive >>= \case
  Just t' | t' == t -> pure ()
  -- If the client responds with an invalid keepalive or one we did not expect, we close the connection
  Just t' -> throwIO $ InvalidKeepAlive t t'
  Nothing -> throwIO UnexpectedKeepAlive

close :: Connection -> IO ()
close Conn{..} = putStrLn "Closed" >> throwTo threadId ConnectionClosed

-- Commands
data Command =
    MoveAndRotateClient !Position !Rotation !Falling
  | MoveClient !Position !Falling
  | RotateClient !Rotation !Falling
  | SetOnGround !Falling
