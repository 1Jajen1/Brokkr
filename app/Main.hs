{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import LCraft (startServer)
import Game.Monad
import Game.State.Internal
import World.Internal
import qualified IO.Chunkloading as Chunkloading
import Control.Exception
import qualified Util.Queue as Queue
import IO.ChunkCache

main :: IO ()
main = do
  st <- newGameState
  catch @SomeException (runGame st $ startServer) $ \e -> do
    print e
    throwIO e

newGameState :: IO GameState
newGameState = do

  overworldCL <- Chunkloading.newFromFolder "server-dir/world/region"

  _worlds <-
    (,,) <$> (World "Overworld" Overworld overworldCL emptyChunkCache <$> Queue.new 16)
         <*> (World "Nether" Nether overworldCL emptyChunkCache <$> Queue.new 16)
         <*> (World "TheEnd" TheEnd overworldCL emptyChunkCache <$> Queue.new 16)
  let _players = mempty
      _connections = mempty
  pure GameState{..}
