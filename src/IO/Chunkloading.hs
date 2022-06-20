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
import qualified Data.HashMap.Strict as HM
import Control.Concurrent
import Data.Foldable (traverse_)
import Control.DeepSeq

data Handle = Handle {
  loadChunk :: ChunkPosition -> IO (Maybe Chunk)
, close     :: IO ()
}

instance NFData Handle where
  rnf !Handle{} = () 

instance Show Handle where
  show _ = "_"

newFromFolder :: String -> IO Handle
newFromFolder path = do
  -- TODO Implement cache eviction for old files
  rFiles <- newMVar mempty
  let loadChunk cp@(ChunkPos x z) = do
        let rX = x `div` 32
            rZ = z `div` 32
        rMap <- takeMVar rFiles
        !rRef <- case HM.lookup (rX, rZ) rMap of
          Just r -> putMVar rFiles rMap >> pure r
          Nothing -> do
            !r <- openRegionFile path rX rZ
            ref <- newMVar r
            putMVar rFiles (HM.insert (rX, rZ) ref rMap)
            pure ref
        
        mchunkBs <- withMVar rRef $ readChunkData cp

        case mchunkBs of
          Nothing -> error $ "Not generated: " <> show cp
          Just chunkBs ->
            -- TODO Adjust this parser and maybe just store Chunk as Packed 'Chunk instead?
            case FP.runParser (get @(BinaryNBT Chunk)) chunkBs of
              FP.OK (BinaryNBT chunk) _ -> pure (Just chunk)
              _ -> error "TODO" -- TODO
      close = do
        rMap <- takeMVar rFiles
        traverse_ (flip withMVar closeRegionFile) rMap
  pure Handle{..}
