{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

import Criterion.Main

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Codec.Compression.GZip as GZip

import qualified FlatParse.Basic as FP

import qualified Mason.Builder as B

import Control.DeepSeq
import GHC.Generics (Generic)

import Util.Binary
import Util.NBT.Internal

data Env = Env {
    bigtestBs :: BS.ByteString
  , bigtestNBT :: NBT
  }
  deriving stock Generic
  deriving anyclass NFData

setupEnv :: IO Env
setupEnv = do
  bigtestBs <- LBS.toStrict . GZip.decompress . LBS.fromStrict <$> BS.readFile "test/Util/NBT/bigtest.nbt"
  let bigtestNBT = case FP.runParser (get @NBT) bigtestBs of
        FP.OK res "" -> res
        _ -> error "Failed to parse NBT"
  return $ Env{..}

main :: IO ()
main = defaultMain [
    env setupEnv $ \ ~Env{..} ->
    bgroup "NBT decoding" $
      [ bench "bigtest.nbt" $ whnf parseBsNBT bigtestBs
      ]
  , env setupEnv $ \ ~Env{..} ->
    bgroup "NBT encoding" $
      [ bench "bigtest.nbt" $ whnf encodeNBT bigtestNBT
      ]
  ]

parseBsNBT :: BS.ByteString -> NBT
parseBsNBT bs = case FP.runParser (get @NBT) bs of
  FP.OK res "" -> res
  _ -> error "Failed to parse NBT"

encodeNBT :: NBT -> BS.ByteString
encodeNBT nbt = B.toStrictByteString (put nbt)

-- orphans
instance NFData NBT where
instance NFData Tag where
