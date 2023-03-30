{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module IO.Chunkloading (
  Chunkloading
, loadChunk, loadChunks
, new
) where

import Chunk.Position

import Control.DeepSeq
import Control.Exception (Exception, SomeException, throwIO, catch, bracket, bracketOnError)

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar

import Control.Monad

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

import Hecs

data Chunkloading = Chunkloading
  ![Async.Async ()] -- Worker threads
  !(MVar ()) -- Workers sleep on this variable
  !(MVar (IntMap (String, Queue Chunkload))) -- TODO Use normal growable array if I have the whole thing enclosed in an MVar anyway
  deriving Component via (ViaBox Chunkloading)

instance NFData Chunkloading where
  rnf (Chunkloading _ _ _) = ()

data Chunkload = Chunkload !ChunkPosition !(MVar Chunk)

loadChunk :: String -> ChunkPosition -> Chunkloading -> IO (MVar Chunk)
loadChunk rPath pos@(ChunkPos x z) (Chunkloading _ sleep ref) = modifyMVar ref $ \im -> do
  ret <- newEmptyMVar
  newM <- case IM.lookup rId im of
    Just (_, q) -> Queue.push q (Chunkload pos ret) >> pure im
    Nothing -> do
      q <- Queue.new 32
      Queue.push q (Chunkload pos ret)
      pure $ IM.insert rId (rPath, q) im

  void $ tryPutMVar sleep () -- wake up any sleeping worker
  pure (newM, ret)
  where
    (rX, rZ) = (x `unsafeShiftR` 5, z `unsafeShiftR` 5)
    rId = ((rX .&. 0xFFFFFFFF) `unsafeShiftL` 32) .|. (rZ .&. 0xFFFFFFFF)

loadChunks :: String -> [ChunkPosition] -> Chunkloading -> (MVar Chunk -> IO ()) -> IO ()
loadChunks _ [] _ _ = pure ()
loadChunks rPath cs (Chunkloading _ sleep ref) f = do
  modifyMVar_ ref $ \im -> foldr singleChunk (pure im) cs
  void $ tryPutMVar sleep () -- wake up any sleeping worker
  where
    singleChunk (pos@(ChunkPos x z)) imM = imM >>= \im -> do
      ret <- newEmptyMVar
      f ret
      case IM.lookup rId im of
        Just (_, q) -> Queue.push q (Chunkload pos ret) >> pure im
        Nothing -> do
          q <- Queue.new 32
          Queue.push q (Chunkload pos ret)
          pure $ IM.insert rId (rPath, q) im
      where
        (rX, rZ) = (x `unsafeShiftR` 5, z `unsafeShiftR` 5)
        rId = ((rX .&. 0xFFFFFFFF) `unsafeShiftL` 32) .|. (rZ .&. 0xFFFFFFFF)
{-# INLINE loadChunks #-}

new :: Int -> IO Chunkloading
new workers = do
  sleep <- newEmptyMVar
  ref <- newMVar mempty
  as <- spawnWorkers sleep ref workers []
  pure $ Chunkloading as sleep ref
  where
    spawnWorkers _ _ 0 xs = pure xs
    spawnWorkers sleep ref n xs = bracketOnError
      (Async.async $ chunkWorker sleep ref n)
      (\a -> Async.cancel a)
      $ \a -> spawnWorkers sleep ref (n - 1) (a:xs)
    chunkWorker sleep ref n = do
      myThreadId >>= flip labelThread ("Chunk worker " <> show n)

      let go = do
            -- TODO Do I need to bracket takeMVar ref?
            im <- takeMVar ref
            case IM.lookupMin im of
              Nothing -> putMVar ref im >> takeMVar sleep >> go
              Just (rId,(rPath, q)) -> do
                -- get all load requests and put the map back so other workers can continue
                ls <- Queue.flush q
                putMVar ref $ IM.delete rId im
                -- figure out which regionfile we need and open it -- TODO Cache?
                let (rX, rZ) = (fromIntegral . fromIntegral @_ @Int32 $ (rId `unsafeShiftR` 32) .&. 0xFFFFFFFF, fromIntegral . fromIntegral @_ @Int32 $ rId .&. 0xFFFFFFFF)
                bracket
                  (RegionFile.openRegionFile rPath rX rZ)
                  (RegionFile.closeRegionFile) $ \rf -> catch (do
                    -- load chunks
                    V.forM_ ls $ \(Chunkload pos ret) -> RegionFile.readChunkData pos rf
                      {- Note: Why do we run decompression and parsing in a separate thread?
                        Decompression is slow. It takes longer than actually parsing the chunk and both together
                          take much much longer than reading from the filesystem.
                        
                        Spawning a thread for each request could also work, but is no more efficient.
                          - Each request has to take a lock on the regionfile, so most requests would wait for each other until
                            the file read is done. This is exactly what we have here, except we only take the lock once for the requests
                      -}
                      (\compType compressedBs -> do
                        as <- Async.async $ do
                          -- TODO I don't actually like this too much. Maybe this should be done in the region file part
                          bs <- case compType of
                            1 -> pure $! LBS.toStrict . GZip.decompress $ LBS.fromStrict compressedBs
                            2 -> pure $! LBS.toStrict . ZLib.decompress $ LBS.fromStrict compressedBs
                            3 -> pure compressedBs
                            _ -> throwIO (UnknownDecompressionFlag pos)
                          case FP.runParser (Binary.get @Chunk) bs of
                            FP.OK c _ -> putMVar ret c
                            _ -> throwIO (ChunkParseFail pos)
                        Async.link as
                      )
                      (throwIO (MissingChunk pos))
                    )  (\(e :: SomeException) -> print e >> throwIO e)
                go
      go

newtype ChunkParseFail = ChunkParseFail ChunkPosition
  deriving stock Show

instance Exception ChunkParseFail

newtype UnknownDecompressionFlag = UnknownDecompressionFlag ChunkPosition
  deriving stock Show

instance Exception UnknownDecompressionFlag

newtype MissingChunk = MissingChunk ChunkPosition
  deriving stock Show

instance Exception MissingChunk
