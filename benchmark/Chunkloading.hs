{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

import Criterion.Main

import Effectful
import Effectful.Reader.Static

import Chunk.Internal
import Effect.Chunkloading
import Effect.World
import Effect.IO.File.Handle
import System.IO (Handle)
import Control.Concurrent

main :: IO ()
main = defaultMain [
    bgroup "Chunkloading" $
      -- 270ms not a bad start! Running this on the network connection is fine, the player will be sent data in < 1 second
      -- This loads 21*21 chunks! ~600 microseconds. This cheats on the file part as the OS likely caches the entire thing
      -- but that is ok, just keep that in mind when comparing later
      [ bench "load a Chunk" $ nfIO loadChunkBench
      ]
  ]

loadChunkBench :: IO [Chunk]
loadChunkBench = do
  traverse (\toLoad -> do
    ref <- newEmptyMVar
    runEff $ runFile $ runReader (RegionFileFolderPath "test/Effect/Chunks/region") $ runChunkloading @Handle $ do
      loadChunk toLoad $ \c -> liftIO $ putMVar ref c
    takeMVar ref
    ) [ChunkPos x y | x <- [-10..10], y <- [-10..10]]
