{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import LCraft (startServer)
import Game.Monad
import Game.State.Internal
import qualified Data.Vector as V
import World.Internal
import qualified IO.Chunkloading as Chunkloading

main :: IO ()
main = do
  st <- newGameState
  runGame st $ startServer

newGameState :: IO GameState
newGameState = do

  overworldCL <- Chunkloading.newFromFolder "server-dir/world/region"

  let _worlds = V.fromListN 3 [
          World "Overworld" Overworld overworldCL
        , World "Nether" Nether overworldCL
        , World "TheEnd" TheEnd overworldCL
        ]
      _players = mempty
      _connections = mempty
  pure GameState{..}
