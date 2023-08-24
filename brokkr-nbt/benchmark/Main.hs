{-# LANGUAGE DerivingStrategies, DeriveAnyClass, RecordWildCards, OverloadedStrings, DataKinds, TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices -dsuppress-all -ddump-simpl #-}
module Main (main) where

import Test.Tasty.Bench

import Control.Exception
import Control.Monad.ST.Strict (runST)

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Data.Int
import Data.Primitive

import Data.Vector.Storable qualified as S

import Codec.Compression.GZip qualified as GZip

import FlatParse.Basic qualified as FP

import Mason.Builder qualified as B

import Control.DeepSeq
import GHC.Generics (Generic)

import Brokkr.NBT.Codec
import Brokkr.NBT.Internal
import Brokkr.NBT.ByteOrder

import BigTest

-- import qualified Data.Serialize as Serialize
-- import qualified Data.Nbt as NBT2

-- TODO Make config option
includeCmp :: Bool
includeCmp = True

instance NFData NBT where
  -- Both arguments to NBT are strict
  -- _key is a newtype around a ByteString which is also fully strict
  -- _tag is also completely strict
  rnf (NBT _key _tag) = ()

data Env = Env {
    envBs      :: !BS.ByteString
  , envNBT     :: !NBT
  -- , envNBT2    :: !NBT2.Nbt'
  , envBigTest :: !BigTest
  }
  deriving stock Generic
  deriving anyclass NFData

setupEnv :: String -> IO Env
setupEnv name = do
  initBs <- BS.readFile ("test/NBT/" ++ name)
  envBs <- handle (\(_ :: SomeException) -> pure initBs) . evaluate . LBS.toStrict . GZip.decompress $ LBS.fromStrict initBs
  let envNBT = case FP.runParser parseNBT envBs of
        FP.OK res "" -> res
        _ -> error "Failed to parse NBT"
  
  bigTestBs0 <- BS.readFile "test/NBT/bigtest.nbt"
  bigTestBs <- handle (\(_ :: SomeException) -> pure initBs) . evaluate . LBS.toStrict . GZip.decompress $ LBS.fromStrict bigTestBs0

  let envBigTest = parseBigTest bigTestBs
      -- envNBT2 = case Serialize.decode envBs of
      --   Left _ -> error "Failed to parse nbt using named-binary-tag"
      --   Right n -> n

  return $ Env{..}

benchFile :: String -> Benchmark
benchFile name =
  env (setupEnv name) $ \ ~Env{..} -> 
  bgroup name $
    [ bench "decode (brokkr)"  $ nf parseBsNBT envBs
    , bench "encode (brokkr)"  $ nf encodeNBT envNBT 
    ]
      <> (if includeCmp then
        [ bench "decode (named-binary-tag)"  $ nf (Serialize.decode @NBT2.Nbt') envBs
        -- , bench "encode (named-binary-tag)"  $ nf (Serialize.runPut . Serialize.put) envNBT2 
        ] else [])
      <> (if name == "bigtest.nbt" then
        [ bench "decode (brokkr) (schema)" $ nf parseBigTest envBs
        , bench "encode (brokkr) (schema)" $ nf encodeBigTest envBigTest
        ]
      else [])

instance NFData b => NFData (NBT2.Nbt b) where
  rnf (NBT2.Nbt k v) = rnf k `seq` rnf v
instance NFData b => NFData (NBT2.Tag b) where
  rnf (NBT2.Byte b) = rnf b
  rnf (NBT2.Short b) = rnf b
  rnf (NBT2.Int b) = rnf b
  rnf (NBT2.Long b) = rnf b
  rnf (NBT2.Float b) = rnf b
  rnf (NBT2.Double b) = rnf b
  rnf (NBT2.String b) = rnf b
  rnf (NBT2.ByteArray b) = rnf b
  rnf (NBT2.IntArray b) = rnf b
  rnf (NBT2.LongArray b) = rnf b
  rnf (NBT2.List b) = rnf b
  rnf (NBT2.Compound b) = rnf b
instance NFData b => NFData (NBT2.Cmpnd b) where
  rnf (NBT2.Cmpnd b v) = rnf v `seq` rnf b

benchByteSwap :: (Num a, S.Storable a) => String -> (S.Vector a -> S.Vector b) -> Benchmark
benchByteSwap n f = bgroup n
  [ env (evaluate . force $ S.generate 4 gen) $ \ ~v ->
    bench "4" $ nf f v
  , env (evaluate . force $ S.generate 16 gen) $ \ ~v ->
    bench "16" $ nf f v
  , env (evaluate . force $ S.generate 128 gen) $ \ ~v ->
    bench "128" $ nf f v
  , env (evaluate . force $ S.generate 400 gen) $ \ ~v ->
    bench "400" $ nf f v
  , env (evaluate . force $ S.generate 4096 gen) $ \ ~v ->
    bench "4096" $ nf f v
  , env (evaluate . force $ S.generate 8000 gen) $ \ ~v ->
    bench "8000" $ nf f v
  , env (evaluate . force $ S.generate 1048576 gen) $ \ ~v ->
    bench "1048576" $ nf f v
  , env (evaluate . force $ S.generate 1500000 gen) $ \ ~v ->
    bench "1500000" $ nf f v
  ]
  where gen i = fromIntegral $ (i * i * 255 + i * 7) `mod` 100

mkRecList :: (NBT, BS.ByteString)
mkRecList =
  let hugeNbt = NBT "" $ nestedList 1000000
      smallArrEmpty = runST $ newSmallArray 0 (error "SmallArr empty") >>= unsafeFreezeSmallArray
      smallArrSingleton x = runST $ newSmallArray 1 x >>= unsafeFreezeSmallArray
      nestedList :: Int -> Tag
      nestedList 0 = TagList smallArrEmpty
      nestedList !n = TagList $ smallArrSingleton $ nestedList (n - 1)
      encodedBs = encodeNBT hugeNbt
  in (hugeNbt, encodedBs)

benchRecList :: Benchmark
benchRecList = env (evaluate . force $ mkRecList) $ \ ~(hugeNbt, hugeBs) -> bgroup "huge recursive list"
  [ bench "decode" $ nf parseBsNBT hugeBs
  , bench "encode" $ nf encodeNBT hugeNbt
  ]

main :: IO ()
main = defaultMain [
    bgroup "nbt files" [
      benchFile "bigtest.nbt"
    , benchFile "complex_player.dat"
    , benchFile "hello_world.nbt"
    , benchFile "inttest3.nbt"
    , benchFile "inttest16.nbt"
    , benchFile "inttest1023.nbt"
    , benchFile "level.dat"
    , benchFile "simple_player.dat"
    ]
  , bgroup "Byteswapping" [
      -- Benchmark byteswap in place
      benchByteSwap @Int32 "bswap32 (unsafe)" unsafeArrSwapBE32
      -- Benchmark copying and byteswapping
      -- First just the copy. This allocates a new vector and copies it
    , benchByteSwap @Int32 @Int32 "memcopy32" (\v -> runST $ S.thaw v >>= S.unsafeFreeze)
      -- Next benchmark copy and byteswap fused. This allocates a new vector
      -- and then copies and byteswaps at the same time
    , benchByteSwap @Int32 "bswap32" arrSwapBE32
      -- Same for 64 bit numbers
    , benchByteSwap @Int64 "bswap64 (unsafe)" unsafeArrSwapBE64
    , benchByteSwap @Int64 @Int64 "memcopy64" (\v -> runST $ S.thaw v >>= S.unsafeFreeze)
    , benchByteSwap @Int64 "bswap64" arrSwapBE64
    ]
  , benchRecList
  -- TODO Add modified-utf-8 validation and conversion benchmarks
  ]

parseBsNBT :: BS.ByteString -> NBT
parseBsNBT !bs = case FP.runParser parseNBT bs of
  FP.OK res "" -> res
  _ -> error "Failed to parse NBT"

encodeNBT :: NBT -> BS.ByteString
encodeNBT !nbt = B.toStrictByteString (putNBT nbt)

parseBigTest :: BS.ByteString -> BigTest
parseBigTest bs = case FP.runParser
  $$(genParser bigTestCodec) bs of
  FP.OK res "" -> res
  FP.Err e -> error $ show e
  _ -> error "Failed to parse NBT"

encodeBigTest :: BigTest -> BS.ByteString
encodeBigTest bt = B.toStrictByteString ($(genBuilder bigTestCodec) bt)
