{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Connection (
  Connection
, new
, SendPacket(..)
-- , executeCommand, executeCommands
, sendPacket, sendPackets
-- , flushCommands
, flushPackets
, sendKeepAlive
, ackKeepAlive
, close
) where

-- import {-# SOURCE #-} Command

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

import Hecs

data Connection = Conn {
  -- TODO Command queue?
  sendLock :: MVar ()
, sendQueue :: Ring SendPacket
, lastKeepAlive :: MVar Int64 -- Full means we sent the number, empty means the client responded correctly
, threadId :: ThreadId
}
  deriving stock Generic
  deriving Component via (ViaBoxed Connection)

data SendPacket = SendPacket !Int !Packet
  deriving stock Show

new :: IO Connection
new = do
  -- commandQueue <- Queue.new 64
  sendLock <- newMVar ()
  sendQueue <- Ring.newRingBuffer 512
  lastKeepAlive <- newEmptyMVar
  threadId <- myThreadId
  pure Conn{..}

-- executeCommand :: Connection -> Command -> IO ()
-- executeCommand Conn{..} c = Queue.push commandQueue c

-- executeCommands :: Connection -> V.Vector Command -> IO ()
-- executeCommands Conn{..} cs = Queue.pushN commandQueue cs

sendPacket :: Connection -> SendPacket -> IO ()
sendPacket Conn{..} p = withMVar sendLock . const $ Ring.push sendQueue p

sendPackets :: Connection -> V.Vector SendPacket -> IO ()
sendPackets Conn{..} ps = withMVar sendLock . const $ Ring.pushN sendQueue ps

-- flushCommands :: Connection -> (V.Vector Command -> IO a) -> IO a
-- flushCommands Conn{..} f = Queue.flush commandQueue >>= f
-- {-# INLINE flushCommands #-}

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
