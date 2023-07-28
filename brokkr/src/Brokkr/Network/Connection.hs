{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Network.Connection (
  Connection
, new
, SendPacket(..)
, UnsafeDefDimHeight
, Encode.Packet(..), Encode.SizeEstimate(..)
, pushCommand, pushCommands
, sendPacket, sendPackets
, flushCommands
, flushPackets
, sendKeepAlive
, ackKeepAlive
, close
) where

import Brokkr.Packet.Encode qualified as Encode
import Brokkr.Packet.ServerToClient.Play

import Brokkr.Network.Command
import Brokkr.Network.Exception (ConnectionClosed(..), ClientTimedOut(..), InvalidKeepAlive(..))

import Brokkr.Util.Queue (Queue)
import Brokkr.Util.Queue qualified as Queue

import Brokkr.Util.Ring (Ring)
import Brokkr.Util.Ring qualified as Ring

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.Int

import qualified Data.Vector as V

import GHC.Generics
import GHC.TypeLits (KnownNat)

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

type UnsafeDefDimHeight = 384

data SendPacket = forall n . (KnownNat (ToHeightMapSize n)) => SendPacket {-# UNPACK #-} !(Encode.Packet (PlayPacket n))

instance Show SendPacket where
  show (SendPacket p) = show p

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

sendPacket :: KnownNat (ToHeightMapSize n) => Connection -> Encode.Packet (PlayPacket n) -> IO ()
sendPacket Conn{..} p = withMVar sendLock . const $ Ring.push sendQueue (SendPacket p)

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
  sendPacket @UnsafeDefDimHeight c . Encode.Packet (Encode.EstimateMin 10) $ KeepAlive t

ackKeepAlive :: Connection -> Int64 -> IO ()
ackKeepAlive Conn{..} t = tryTakeMVar lastKeepAlive >>= \case
  Just t' | t' == t -> pure ()
  -- If the client responds with an invalid keepalive or one we did not expect, we close the connection
  Just t' -> throwIO $ InvalidKeepAlive t t'
  Nothing -> throwIO UnexpectedKeepAlive

close :: Connection -> IO ()
close Conn{..} = putStrLn "Closed" >> throwTo threadId ConnectionClosed
