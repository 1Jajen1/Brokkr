{-# LANGUAGE UndecidableInstances #-}
module IO.Chunkloading (
  Chunkloading
, loadChunk, loadChunks
, new
) where

import Chunk.Position

import Control.DeepSeq
import Control.Exception (Exception, SomeException, throwIO, catch, bracket, evaluate, finally)

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

import GHC.Conc (myThreadId, labelThread, unsafeIOToSTM)

import qualified IO.RegionFile as RegionFile

import qualified Util.Binary as Binary
import Util.Queue (Queue)
import qualified Util.Queue as Queue

import IO.Chunk
import IO.ChunkParser ()

import Hecs
import Control.Concurrent.STM
import Control.Concurrent.Async (async)

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
    singleChunk pos@(ChunkPos x z) imM = imM >>= \im -> do
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
  liveRef <- newTVarIO mempty
  as <- spawnWorkers sleep ref liveRef workers []
  pure $ Chunkloading as sleep ref
  where
    spawnWorkers _ _ _ 0 xs = pure xs
    spawnWorkers sleep ref liveRef n xs = do
      a <- Async.async $ chunkWorker sleep ref liveRef n
      -- Just crash the server, broken chunkloading
      -- does more harm than good if left running
      Async.link a
      spawnWorkers sleep ref liveRef (n - 1) (a:xs)
    chunkWorker sleep ref liveRef n = do
      myThreadId >>= flip labelThread ("Chunk worker " <> show n)

      let go = do
            -- TODO Do I need to bracket takeMVar ref?
            -- All paths to putMVar should be pure and exception
            -- free right?
            im <- takeMVar ref
            case IM.lookupMin im of
              Nothing -> putMVar ref im >> takeMVar sleep >> go
              Just (rId,(rPath, q)) -> do
                -- get all load requests and put the map back so other workers can continue
                ls <- Queue.flush q
                putMVar ref $ IM.delete rId im
                withRegionFile liveRef rPath rId $ \rf -> catch (do
                    -- load chunks
                    V.forM_ ls $ \(Chunkload pos ret) -> RegionFile.readChunkData pos rf
                      {- Note: Why do we run decompression and parsing in a separate thread?
                        Decompression is slow. It takes longer than actually parsing the chunk and both together
                          take much much longer than reading from the filesystem.

                        Spawning a thread for each request could also work, but is no more efficient.
                          - Each request has to take a lock on the regionfile, so requests would wait for each other until
                            the file read is done. This is exactly what we have here, except we only take the lock once for the requests
                          - Ideally we'd submit requests to the file and don't block until we get the response
                      -}
                      (\compType compressedBs -> do
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
                      )
                      (throwIO (MissingChunk pos))
                    )  (\(e :: SomeException) -> do
                      -- print e
                      throwIO e)
                go
      go

-- TODO Close regionfiles again when no longer needed
newtype LiveRegionFiles = LiveRegionFiles (IntMap (MVar RegionFile.RegionFile, TVar Int))
  deriving newtype (Semigroup, Monoid)

withRegionFile :: TVar LiveRegionFiles -> String -> Int -> (RegionFile.RegionFile -> IO a) -> IO a
withRegionFile refs rPath rId f = do
  -- STM makes this otherwise complicated cache trivial:
  -- We keep a 'RegionFile' alive as long as there is demand for it. A counter keeps
  -- track of everyone waiting on the 'MVar' and once that hits 0
  -- 'JoinGameSpec' is most likely to catch races here, as it did catch all of them
  -- before I rewrote with 'STM' because it validates that all required chunks were delivered.
  atomically (do
    LiveRegionFiles live <- readTVar refs
    case IM.lookup rId live of
      Just (regionRef,refC) -> do
        -- Increment the demand counter and return the mvar and counter for use
        modifyTVar' refC (+ 1)
        pure $ Left (regionRef,refC)
      Nothing -> do
        -- Create a new MVar and demand coutner pair and add them to the map
        -- This is a safe use of 'unsafeIOToSTM' as it has no observable side-effects
        -- outside of allocation. We could avoid this with 'TMVar', but I want fairness
        -- here. 'STM' wakes up all waiting transactions at once, 'MVar' in order. So
        -- if we were to use 'TMVar', threads may be starved out by scheduling.
        mv <- unsafeIOToSTM $ newEmptyMVar
        refC <- newTVar 1
        writeTVar refs $! LiveRegionFiles $ IM.insert rId (mv,refC) live
        pure $ Right (mv,refC)
        ) >>= \case
    -- Wait on the MVar and don't forget to decrement the counter later
    Left (mv,refC) -> flip finally (atomically $ modifyTVar' refC (\i -> i - 1)) $ withMVar mv f
    -- Need to create a new regionfile
    Right (mv,refC) -> do
      -- Spawn a kill thread. When there is no more demand on the file we close it
      -- Has no races because we also remove the entry from the map in the same transaction
      void . async $ do
        atomically $ readTVar refC >>= \case
          -- Remove the entry for the regionfile
          0 -> modifyTVar' refs $ \(LiveRegionFiles live) -> LiveRegionFiles $ IM.delete rId live
          _ -> retry
        -- Nobody but us has a reference to the MVar so we can safely take and close it
        takeMVar mv >>= RegionFile.closeRegionFile

      let (rX, rZ) = (fromIntegral . fromIntegral @_ @Int32 $ (rId `unsafeShiftR` 32) .&. 0xFFFFFFFF, fromIntegral . fromIntegral @_ @Int32 $ rId .&. 0xFFFFFFFF)
      bracket
        (RegionFile.openRegionFile rPath rX rZ)
        (\rf -> do
          putMVar mv rf
          atomically $ modifyTVar' refC (\i -> i - 1)
          )
        f

data ChunkParseFail = ChunkParseFail ChunkPosition (Maybe SomeException)
  deriving stock Show

instance Exception ChunkParseFail

newtype UnknownDecompressionFlag = UnknownDecompressionFlag ChunkPosition
  deriving stock Show

instance Exception UnknownDecompressionFlag

newtype MissingChunk = MissingChunk ChunkPosition
  deriving stock Show

instance Exception MissingChunk
