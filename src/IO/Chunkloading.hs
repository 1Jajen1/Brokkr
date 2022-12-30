{-# LANGUAGE RecordWildCards #-}
module IO.Chunkloading (
  Chunkloader
, loadChunk
) where

import Chunk.Position

data Chunkloader = Chunkloader {
  regionFileFolder :: FilePath
}

data Chunk

loadChunk :: Chunkloader -> ChunkPosition -> IO Chunk
loadChunk Chunkloader{..} cp = do
  pure undefined

{- TODO:

-- Define an IOChunk, could also be multiversion
-- Define IOChunk -> IO Chunk conversion


-}
