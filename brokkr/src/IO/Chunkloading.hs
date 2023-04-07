{-# LANGUAGE UndecidableInstances #-}
module IO.Chunkloading (
  Chunkloading
, loadChunk, loadChunks
, new
) where

import Chunk.Position

import Control.DeepSeq
import Control.Exception

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar

import Data.Bits
import Data.Int
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.Zlib as ZLib
import qualified Codec.Compression.GZip as GZip

import qualified FlatParse.Basic as FP

import GHC.Conc (myThreadId, labelThread)

import qualified IO.RegionFile as RegionFile

import qualified Util.Binary as Binary
import Util.Queue (Queue)
import qualified Util.Queue as Queue

import IO.Chunk
import IO.ChunkParser ()

import Hecs
import Control.Concurrent.STM
import Control.Monad.Fix
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.Chan qualified as Chan

data Chunkloading = Chunkloading
  ![Async.Async ()] -- Worker threads
  !(Chan WorkRequest)
  !(MVar (IntMap WorkRequest)) -- TODO Use normal growable array if I have the whole thing enclosed in an MVar anyway
  deriving Component via (ViaBox Chunkloading)

data WorkRequest = WorkRequest !Int !String !(Queue Chunkload)

instance NFData Chunkloading where
  rnf Chunkloading{} = ()

data Chunkload = Chunkload !ChunkPosition !(MVar Chunk)

loadChunk :: String -> ChunkPosition -> Chunkloading -> IO (MVar Chunk)
loadChunk rPath pos@(ChunkPos x z) (Chunkloading _ workQ ref) = modifyMVar ref $ \im -> do
  ret <- newEmptyMVar
  !newM <- case IM.lookup rId im of
    Just (WorkRequest _ _ q) -> Queue.push q (Chunkload pos ret) >> pure im
    Nothing -> do
      q <- Queue.new 32
      Queue.push q (Chunkload pos ret)
      let req = WorkRequest rId rPath q
      Chan.writeChan workQ req
      pure $ IM.insert rId req im
  pure (newM, ret)
  where
    (rX, rZ) = (x `unsafeShiftR` 5, z `unsafeShiftR` 5)
    rId = ((rX .&. 0xFFFFFFFF) `unsafeShiftL` 32) .|. (rZ .&. 0xFFFFFFFF)

-- TODO Better impl
loadChunks :: String -> [ChunkPosition] -> Chunkloading -> (MVar Chunk -> IO ()) -> IO ()
loadChunks _ [] _ _ = pure ()
loadChunks rPath cs (Chunkloading _ workQ ref) f = do
  modifyMVar_ ref $ \im -> foldr singleChunk (pure im) cs
  where
    singleChunk pos@(ChunkPos x z) imM = imM >>= \im -> do
      ret <- newEmptyMVar
      f ret
      case IM.lookup rId im of
        Just (WorkRequest _ _ q) -> Queue.push q (Chunkload pos ret) >> pure im
        Nothing -> do
          q <- Queue.new 32
          Queue.push q (Chunkload pos ret)
          let req = WorkRequest rId rPath q
          Chan.writeChan workQ req
          pure $ IM.insert rId req im
      where
        (rX, rZ) = (x `unsafeShiftR` 5, z `unsafeShiftR` 5)
        rId = ((rX .&. 0xFFFFFFFF) `unsafeShiftL` 32) .|. (rZ .&. 0xFFFFFFFF)
{-# INLINE loadChunks #-}

new :: Int -> IO Chunkloading
new workers = do
  ref <- newMVar mempty
  workQ <- Chan.newChan
  as <- spawnWorkers workQ ref workers []
  pure $ Chunkloading as workQ ref
  where
    spawnWorkers _ _ 0 xs = pure xs
    spawnWorkers workQ ref n xs = do
      a <- Async.async $ chunkWorker workQ ref n
      -- Just crash the server, broken chunkloading
      -- does more harm than good if left running
      Async.link a
      spawnWorkers workQ ref (n - 1) (a:xs)
    chunkWorker ref liveRef n = do
      myThreadId >>= flip labelThread ("Chunk worker " <> show n)
      fix $ \goWorker -> Chan.readChan ref >>= \(WorkRequest rId rPath q) -> do
        let (rX, rZ) = (fromIntegral . fromIntegral @_ @Int32 $ (rId `unsafeShiftR` 32) .&. 0xFFFFFFFF, fromIntegral . fromIntegral @_ @Int32 $ rId .&. 0xFFFFFFFF)
        -- We only need to close on error, the normal path out of the loop already handles closing (and it has to)
        -- Should something throw after closing, we close twice, but that should be safe
        bracketOnError (RegionFile.openRegionFile rPath rX rZ) RegionFile.closeRegionFile $ \rf ->
          handle
              (\(e :: SomeException) -> throwIO e)
            . fix $ \goQueue -> Queue.flush q >>= \ls -> do
              if V.null ls
                then mask $ \restore ->
                  takeMVar liveRef >>= \case
                    -- We hold the lock, now check one last time that the queue is indeed empty
                    -- TODO What does it mean if Queue.isEmpty throws? It probably can't?
                    lr -> (restore (Queue.isEmpty q) `onException` putMVar liveRef lr) >>= \case
                      -- Someone put something into our queue right after we thought we were done
                      False -> putMVar liveRef lr >> restore goQueue
                      -- We are indeed done, put the intmap back without this region file and queue and
                      -- close the regionfile 
                      True -> do
                        -- First close the regionfile, then put back the intmap, so that nobody else will try to open
                        -- the file before it was closed
                        -- We technically don't care if closeRegionFile throws because that will crash the server, but
                        -- should that ever change, we always need to put the intmap back!
                        RegionFile.closeRegionFile rf `finally` (putMVar liveRef $! IM.delete rId lr)
                else do
                  V.forM_ ls $ \(Chunkload pos ret) -> flip (RegionFile.readChunkData pos rf) (throwIO (MissingChunk pos))
                      {- Note: Why do we run decompression and parsing in a separate thread?
                        Decompression is slow. It takes longer than actually parsing the chunk and both together
                          take much much longer than reading from the filesystem.

                        Spawning a thread for each request could also work, but is no more efficient.
                          - Each request has to take a lock on the regionfile, so requests would wait for each other until
                            the file read is done. This is exactly what we have here, except we only take the lock once for the requests
                          - Ideally we'd submit requests to the file and don't block until we get the response
                      -}
                      $ \compType compressedBs -> do
                        as <- Async.async $ do
                          -- TODO I don't actually like this too much. Maybe this should be done in the region file part
                          bs <- case compType of
                            1 -> pure $! LBS.toStrict . GZip.decompress $ LBS.fromStrict compressedBs
                            2 -> pure $! LBS.toStrict . ZLib.decompress $ LBS.fromStrict compressedBs
                            3 -> pure $! compressedBs
                            _ -> throwIO (UnknownDecompressionFlag pos)
                          
                          !res <- catch (evaluate $ FP.runParser (Binary.get @Chunk) bs) $ \(e :: SomeException) -> do
                            -- print e
                            throwIO (ChunkParseFail pos $ Just e)

                          -- case FP.runParser (Binary.get @Chunk) bs of
                          case res of
                            FP.OK !c _ -> putMVar ret c
                            !_ -> throwIO (ChunkParseFail pos Nothing)
                        Async.link as
                  -- Try to read more requests from the queue
                  goQueue
        goWorker

-- TODO Close regionfiles again when no longer needed
newtype LiveRegionFiles = LiveRegionFiles (IntMap (MVar RegionFile.RegionFile, TVar Int))
  deriving newtype (Semigroup, Monoid)

data ChunkParseFail = ChunkParseFail ChunkPosition (Maybe SomeException)
  deriving stock Show

instance Exception ChunkParseFail

newtype UnknownDecompressionFlag = UnknownDecompressionFlag ChunkPosition
  deriving stock Show

instance Exception UnknownDecompressionFlag

newtype MissingChunk = MissingChunk ChunkPosition
  deriving stock Show

instance Exception MissingChunk
