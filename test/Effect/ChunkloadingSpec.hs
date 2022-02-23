{-# LANGUAGE RecordWildCards #-}
module Effect.ChunkloadingSpec where

import Test.Syd

import Effectful
import Effectful.Reader.Static

import Chunk.Internal
import Effect.Chunkloading
import Effect.World
import Effect.IO.File.Handle
import System.IO (Handle)
import Control.Concurrent

spec :: Spec
spec = do
  describe "Chunkloading" $ do
    describe "loadChunk" $ do
      it "should return the correct chunk" $ do
        let toLoad = ChunkPos 0 0
        ref <- newEmptyMVar
        runEff $ runFile $ runReader (RegionFileFolderPath "test/Effect/Chunks/region") $ runChunkloading @Handle $ do
          loadChunk toLoad $ \c -> liftIO $ putMVar ref c
        Chunk{..} <- takeMVar ref
        position `shouldBe` toLoad
