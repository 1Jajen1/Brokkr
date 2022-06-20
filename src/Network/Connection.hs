{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Network.Connection (
  Handle(
    sendPacket
  , sendPackets
  )
, new
) where

import Network.Connection.Internal
import Network.Protocol
import Network.Util.Builder

import Util.Binary
import qualified Util.Ring as Ring

import qualified Data.Vector as V
import Control.Concurrent
import Data.ByteString.Lazy hiding (last)
import qualified Data.Vector.Mutable as MV
import qualified Data.Time.Clock.POSIX as Time
import qualified Network.Packet.Client.Play as C
import Prelude hiding (last)
import qualified Data.Time as Time
import Data.IORef
import Data.UUID

-- TODO Is the send function here problematic for perf? I mean it is pointer passing and very small so ...
new :: (ByteString -> IO ()) -> UUID -> Protocol -> IO Handle
new send uid prot = do
  ring <- Ring.newRingBuffer 512 -- This has to be larger than the max chunk batch size for one conn, so ~ (viewDistance * 2) ** 2
  sendLock <- newMVar ()
  -- TODO use a specialized structure for the queue
  commandQueue <- MV.unsafeNew 64 >>= newIORef
  cmdQueueInd <- newMVar 0 -- TODO This is rather unsafe if we crash the connection at the wrong time

  let sendPacket p   = withMVar sendLock . const $ Ring.push ring p
      sendPackets ps = withMVar sendLock . const $ Ring.pushN ring ps
      pushCommand c = do
        ind <- takeMVar cmdQueueInd
        q <- readIORef commandQueue

        q' <- if ind >= MV.length q
          then MV.grow q $ MV.length q * 2
          else pure q
        
        -- TODO unsafewrite
        MV.unsafeWrite q' ind c

        writeIORef commandQueue q'
        
        putMVar cmdQueueInd $ ind + 1
      pushCommands cs = do
        mcs <- V.unsafeThaw cs
        ind <- takeMVar cmdQueueInd

        q <- readIORef commandQueue

        -- TODO Better growing strat
        q' <- if ind + V.length cs > MV.length q
          then MV.grow q $ min (MV.length q + V.length cs) $ MV.length q * 2
          else pure q

        MV.unsafeCopy (MV.unsafeSlice ind (V.length cs) q') mcs

        writeIORef commandQueue q'
        
        putMVar cmdQueueInd $ ind + V.length cs
      flushCommands = do
        ind <- takeMVar cmdQueueInd
        q <- readIORef commandQueue
        let slice = MV.unsafeSlice 0 ind q
        ret <- V.freeze slice
        MV.clear q
        putMVar cmdQueueInd 0 >> pure ret
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

        send . fromChunks $! fmap (\(szHint, packet) -> toStrictSizePrefixedByteString prot szHint $ put packet) (V.toList toSend)

        Ring.advanceN ring $ fromIntegral (V.length toSend)

        goSend


  -- This should get shut down when the Handle is GC'd right?
  _ <- forkIO goSend
  
  now <- (\n -> floor $ 1e3 * n) . Time.nominalDiffTimeToSeconds <$> Time.getPOSIXTime
  _ <- forkIO $ keepAlive $ now - timeBetweenKeepAlive

  pure Handle{..}
{-# INLINE new #-}
