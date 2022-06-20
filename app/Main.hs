{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import LCraft (startServer)
import Game.Monad
import Game.State.Internal
import World.Internal
import qualified IO.Chunkloading as Chunkloading
import Control.Exception

main :: IO ()
main = do
  st <- newGameState
  catch @SomeException (runGame st $ startServer) $ \e -> do
    print e
    throwIO e

newGameState :: IO GameState
newGameState = do

  overworldCL <- Chunkloading.newFromFolder "server-dir/world/region"

  let _worlds = (
          World "Overworld" Overworld overworldCL mempty
        , World "Nether" Nether overworldCL mempty
        , World "TheEnd" TheEnd overworldCL mempty
        )
      _players = mempty
      _connections = mempty
  pure GameState{..}
