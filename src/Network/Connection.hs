{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Network.Connection (
  Handle(
    sendPacket
  , sendPackets
  )
, DisconnectException(..)
, new
, pushCommand
, pushCommands
, flushCommands
) where

import Network.Connection.Internal
import Network.Protocol
import Network.Util.Builder

import Util.Binary
import qualified Util.Ring as Ring

import qualified Data.Vector as V
import Control.Concurrent
import Data.ByteString.Lazy hiding (last)
import qualified Data.Time.Clock.POSIX as Time
import qualified Network.Packet.Client.Play as C
import Prelude hiding (last)
import qualified Data.Time as Time
import Data.UUID
import Control.Concurrent.Async
import GHC.Conc (labelThread)
import qualified Data.Text as T
import qualified Util.Queue as Queue
import Command.Connection (Command)

-- TODO Is the send function here problematic for perf? I mean it is pointer passing and very small so ...
new :: (ByteString -> IO ()) -> UUID -> Protocol -> IO Handle
new send uid prot = do
  ring <- Ring.newRingBuffer 512 -- This has to be larger than the max chunk batch size for one conn, so ~ (viewDistance * 2) ** 2
  sendLock <- newMVar ()
  -- TODO use a specialized structure for the queue
  commands <- Queue.new 64
  let sendPacket p   = withMVar sendLock . const $ Ring.push ring p
      sendPackets ps = withMVar sendLock . const $ Ring.pushN ring ps
      showHandle = "" -- TODO add some information here?
      keepAlive :: Int -> IO ()
      keepAlive !last = do
        now <- (\n -> floor $ 1e3 * n) . Time.nominalDiffTimeToSeconds <$> Time.getPOSIXTime
        let delta = (timeBetweenKeepAlive * 2) - now + last
        threadDelay . max 0 $ delta * 1000
        sendPacket (10, C.KeepAlive $ fromIntegral now)
        keepAlive now
      timeBetweenKeepAlive = 20 * 1000
      goSend = do
        !toSend <- Ring.peekN ring

        -- TODO This should be safe from async exceptions
        -- Cutting of the connection mid send may crash the client and is anything but graceful
        send . fromChunks $ fmap (\(szHint, packet) -> let !bs = toStrictSizePrefixedByteString prot szHint $ put packet in bs) (V.toList toSend)

        Ring.advanceN ring $ fromIntegral (V.length toSend)

        goSend

  -- This should get shut down when the Handle is GC'd right?
  connSender <- async $ do
    myThreadId >>= flip labelThread ("Connection sender for " <> show uid)
    goSend >> pure ()
  -- TODO Does link rethrow Cancellation exceptions?
  link connSender
  
  now <- (\n -> floor $ 1e3 * n) . Time.nominalDiffTimeToSeconds <$> Time.getPOSIXTime
  connKeepAlive <- async $ do
    myThreadId >>= flip labelThread ("Connection keepAlive for " <> show uid)
    keepAlive $ now - timeBetweenKeepAlive
  link connKeepAlive

  connThread <- myThreadId
  let close = do
        cancel connSender
        cancel connKeepAlive
      disconnect reason = do
        close
        throwTo connThread DisconnectException
        send $ fromStrict $! toStrictSizePrefixedByteString prot szEst $ put (C.Disconnect reason)
        where szEst = 1 + T.length reason -- TODO Better estimate? This is fine for ascii only chars

  pure Handle{..}
{-# INLINE new #-}


pushCommand :: Handle -> Command -> IO ()
pushCommand hdl c = Queue.push (commands hdl) c
pushCommands :: Handle -> V.Vector Command -> IO ()
pushCommands hdl cs = Queue.pushN (commands hdl) cs
flushCommands :: Handle -> IO (V.Vector Command)
flushCommands hdl = Queue.flush (commands hdl)
