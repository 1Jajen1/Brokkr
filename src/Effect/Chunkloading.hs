{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Effect.Chunkloading (
  Chunkloading(..)
, loadChunk
, runChunkloading
) where

import Effectful

import Chunk.Internal
import Effectful.Dispatch.Dynamic
import Effect.IO.File
import qualified Effect.IO.RegionFile as RegionFile
import Effect.World (RegionFileFolderPath)
import Effectful.Reader.Static
import qualified FlatParse.Basic as FP
import Util.NBT
import Util.Binary (get)
import Effect.Token

type instance DispatchOf (Chunkloading w) = 'Dynamic

data Chunkloading (w :: k) :: Effect where
  LoadChunk :: ~(Token w) -> ChunkPosition -> (Chunk -> m ()) -> Chunkloading w m ()

loadChunk :: Chunkloading w :> es => Token w -> ChunkPosition -> (Chunk -> Eff es ()) -> Eff es ()
loadChunk tok pos act = send $ LoadChunk tok pos act

runChunkloading ::
  forall file w es a .
  ( File file :> es
  , Reader (RegionFileFolderPath w) :> es
  ) => Eff (Chunkloading w : es) a -> Eff es a
runChunkloading = interpret $ \env -> \case
  LoadChunk tok cp@(ChunkPos x z) f -> do
    regionFile <- RegionFile.openRegionFile @file tok (x `div` 32) (z `div` 32)
    
    chunkBs <- RegionFile.readChunkData cp regionFile

    case FP.runParser (get @(BinaryNBT Chunk)) chunkBs of
      FP.OK (BinaryNBT chunk) _ -> localSeqUnlift env $ \hdl -> hdl (f chunk)
      _ -> error "TODO" -- TODO
