{-# LANGUAGE DeriveAnyClass #-}
module ChunkLoadSpec (
  spec
) where

import Test.Syd

import Brokkr.Anvil.Chunk
import Brokkr.Anvil.Chunk.Parser
import Brokkr.Anvil.RegionFile

import Codec.Compression.Zlib qualified as ZLib
import Codec.Compression.GZip qualified as GZip

import Control.Exception
import Control.Monad

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Data.Int

import FlatParse.Basic qualified as FP

spec :: Spec
spec = do
  around (bracket (openRegionFile "./test" 0 0) closeRegionFile) $ do
    -- TODO Get a more diverse region file
    it "should load the regionFile" $ \rf -> do
      let toLoad = [(x,z) | x <- [0..31], z <- [0..31]]
      forM_ toLoad $ \(x, z) -> do
        bs <- flip (readChunkData (ChunkPosition x z) rf) (throwIO $ ReadBytesFailed x z) $ \cty compressedBs -> case cty of
          1 -> pure $! LBS.toStrict . GZip.decompress $ LBS.fromStrict compressedBs
          2 -> pure $! LBS.toStrict . ZLib.decompress $ LBS.fromStrict compressedBs
          3 -> pure $! compressedBs
        !res <- catch (evaluate $ FP.runParser parseChunkNBT bs) $ \(e :: SomeException) -> do
          throwIO (FailedToLoad x z $ Just e)
        case res of
          FP.OK c remBs | BS.null remBs -> void $ evaluate c
          FP.Err e -> throwIO (FailedToLoad x z (Just $ SomeException e))
          _        -> throwIO (FailedToLoad x z Nothing)

data ReadBytesFailed = ReadBytesFailed Int32 Int32
  deriving stock Show
  deriving anyclass Exception

data FailedToLoad = FailedToLoad Int32 Int32 (Maybe SomeException)
  deriving stock Show
  deriving anyclass Exception
