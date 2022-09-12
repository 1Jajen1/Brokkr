{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
module IO.Chunkloading (
  Handle(..)
, newFromFolder
) where

import Chunk.Internal
import IO.RegionFile
import qualified FlatParse.Basic as FP
import Util.NBT
import Util.Binary
import qualified Data.IntMap as IM
import Control.Concurrent
import Data.Foldable (traverse_)
import Control.DeepSeq
import qualified Util.Ring as Ring
import Control.Monad.Fix
import Control.Exception
import Control.Concurrent.Async
import qualified Data.Vector as V
import Data.Bits
import Control.Monad (forM)
import GHC.Conc (numCapabilities, labelThread)

data Handle = Handle {
  loadChunk  :: ChunkPosition -> (Maybe Chunk -> IO ()) -> IO ()
, loadChunks :: V.Vector ChunkPosition -> (Maybe Chunk -> IO ()) -> IO ()
, close     :: IO ()
}

instance NFData Handle where
  rnf !Handle{} = () 

instance Show Handle where
  show _ = "_"

data ChunkReq = CR !ChunkPosition !(Maybe Chunk -> IO ())

data ChunkWorkerCrashed = ChunkWorkerCrashed !String !Int !Int
  deriving stock Show
instance Exception ChunkWorkerCrashed where

newFromFolder :: String -> IO Handle
newFromFolder path = do
  -- TODO A way to unload regionFiles
  -- Probably just LRU and a fixed size here
  let ringCap = 1024
      numWorkers = numCapabilities
  !reqs <- Ring.newRingBuffer ringCap
  ringLock <- newMVar ()
  rFiles <- newMVar mempty
  let withQueue rX rZ act = do
        let msk = (1 `unsafeShiftL` 32) - 1
            key = (msk .&. rX) .|. (rZ `unsafeShiftL` 32)
        modifyMVar rFiles $ \rMap -> do
          case IM.lookup key rMap of
            Just rVar -> (rMap,) <$> act rVar
            Nothing -> do
              rF <- openRegionFile path rX rZ
              (IM.insert key rF rMap,) <$> act rF
      loadChunk cp act = Ring.push reqs $ CR cp act
      loadChunks cps act =
        -- TODO: This can block indefinitely if we try to load more than ringCap chunks at once
        Ring.pushN reqs $ (\cp -> CR cp act) <$> cps

  workers <- forM [0..numWorkers] $ \n -> async $ do
    myThreadId >>= flip labelThread ("Chunkloading worker " <> show n)
    fix $ \loop -> do
      !(CR cp@(ChunkPos x z) act) <- withMVar ringLock . const $ Ring.take reqs
      let rX = x `div` 32
          rZ = z `div` 32
      withQueue rX rZ (readChunkData cp) >>= \case
        Nothing -> do
          throwIO $ ChunkWorkerCrashed "Chunk not in regionfile" rX rZ
        Just !chunkBs ->
          -- TODO Parse this into an IOChunk, which is just a newtype around a foreign ptr
          -- Then parse that into the real chunk. This allows me to bypass NBT completely
          case FP.runParser (get @(BinaryNBT Chunk)) chunkBs of
            FP.OK (BinaryNBT !chunk) _ -> act (Just chunk) >> loop
            _ -> do
              throwIO $ ChunkWorkerCrashed "Failed to parse chunk" rX rZ

  traverse_ link workers

  let close = do
        traverse_ cancel workers
        rMap <- takeMVar rFiles
        traverse_ closeRegionFile rMap

  -- let withQueue rX rZ act = do
  --       modifyMVar rFiles $ \rMap -> do
  --         (queue, newMap) <- case HM.lookup (rX, rZ) rMap of
  --           Just (_, queue) -> pure (queue, rMap)
  --           Nothing -> do
  --             !r <- Ring.newRingBuffer 512 
  --             -- TODO Catch errors in the forked thread
  --             as <- async $ bracket (openRegionFile path rX rZ) (\r' -> closeRegionFile r') $ \rF ->
  --               fix $ \loop -> do
  --                 !(CR cp' ch) <- Ring.take r
  --                 mchunkBs <- readChunkData cp' rF
  --                 case mchunkBs of
  --                   Nothing -> do
  --                     throwTo tId $ ChunkWorkerCrashed "Chunk not in regionfile" rX rZ
  --                     error $ "Not generated: " <> show cp'
  --                   Just chunkBs ->
  --                     case FP.runParser (get @(BinaryNBT Chunk)) chunkBs of
  --                       FP.OK (BinaryNBT chunk) _ -> ch (Just chunk) >> loop
  --                       _ -> do
  --                         throwTo tId $ ChunkWorkerCrashed "Failed to parse chunk" rX rZ
  --                         error "TODO"
  --             ref <- newMVar r
  --             pure (ref, HM.insert (rX, rZ) (as, ref) rMap)
  --         res <- withMVar queue $ \ring -> act ring
  --         pure (newMap, res)
  --     {-# INLINE withQueue #-}
  --     loadChunk cp@(ChunkPos x z) act = do
  --       let rX = x `div` 32
  --           rZ = z `div` 32
  --       withQueue rX rZ $ \q -> do
  --         ret <- newEmptyMVar
  --         Ring.push q $! CR cp $ \chunk -> do
  --           a <- act chunk
  --           putMVar ret a
  --         takeMVar ret
  --     loadChunks cps act = do
  --       vs <- V.forM cps $ \cp@(ChunkPos x z) -> do
  --         let rX = x `div` 32
  --             rZ = z `div` 32
  --         withQueue rX rZ $ \q -> do
  --           ret <- newEmptyMVar
  --           Ring.push q $! CR cp $ \chunk -> do
  --             a <- act chunk
  --             putMVar ret a
  --           pure ret
  --       traverse takeMVar vs
  --     close = do
  --       rMap <- takeMVar rFiles
  --       traverse_ (cancel . fst) rMap
  pure Handle{..}

