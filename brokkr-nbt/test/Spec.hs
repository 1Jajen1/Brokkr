{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Prelude hiding (readFile)

import BigTest

import Control.Exception hiding (assert)
import Control.Monad (when)
import Control.Monad.ST.Strict

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Data.Coerce
import Data.List (sortOn)

import Data.Primitive

import Data.Vector.Storable qualified as S

import Codec.Compression.GZip qualified as GZip

import FlatParse.Basic qualified as FP

import Mason.Builder qualified as B

import Brokkr.NBT.ByteOrder
import Brokkr.NBT.Internal
import Brokkr.NBT.NBTString.Internal
import Brokkr.NBT.Codec

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR

import CodecSpec
import ModifiedUtf8Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "NBT"
  [ testFiles
  , testModifiedUtf8
  , testProperty "NBT roundtrips" . H.property $ do
      nbt <- H.forAll genNBT
      H.tripping nbt encodeNBT decodeNBT
  , testRecursiveNBT
  , testCodec
  ]

testRecursiveNBT :: TestTree
testRecursiveNBT = testCaseSteps "recursive" $ \out -> do
  let hugeNbt = NBT "" $ nestedList 1000000
      -- smallArrEmpty = runST $ newSmallArray 0 (error "SmallArr empty") >>= unsafeFreezeSmallArray
      smallArrSingleton x = runST $ newSmallArray 1 x >>= unsafeFreezeSmallArray
      nestedList :: Int -> Tag
      nestedList 0 = TagList emptySmallArray
      nestedList !n = TagList $ smallArrSingleton $ nestedList (n - 1)
      encodedBs = encodeNBT hugeNbt
  
  seq encodedBs $ pure ()
  out "Writing worked"
  case decodeNBT encodedBs of
    Nothing -> out "reading failed"
    Just hugeNbt1 -> do
      out "reading worked"
      assertEqual "Eq nbt" hugeNbt hugeNbt1  

testFiles :: TestTree
testFiles = testGroup "Files"
  [ testFile "bigtest.nbt"
  , testFile "complex_player.dat"
  , testFile "hello_world.nbt"
  , testFile "inttest3.nbt"
  , testFile "inttest16.nbt"
  , testFile "inttest1023.nbt"
  , testFile "level.dat"
  , testFile "simple_player.dat"
  , testFile "realworld.nbt"
  ]

testFile :: String -> TestTree
testFile name = testCaseSteps name $ \out -> do
  fileBS <- readFile name
  
  nbt0 <- case decodeNBT fileBS of
    Just x -> pure x
    Nothing -> assertFailure "Invalid nbt (initial parse)"

  let encodedBS = encodeNBT nbt0

  -- we cannot test for actual equality because the order of compounds may change
  assertEqual "Encoded nbt is equal in size" (BS.length encodedBS) (BS.length fileBS)

  -- re-parse and check if we get the same input
  nbt1@(NBT _ t1) <- case decodeNBT encodedBS of
    Just x -> pure x
    Nothing -> assertFailure "Invalid nbt (parse from encoded)"
  
  when (name == "bigtest.nbt") $ do
    bt <- case FP.runParser ($$(genParser bigTestCodec)) fileBS of
      FP.OK res remBs | BS.null remBs -> evaluate res
      FP.Err e -> assertFailure $ show e
      _ -> assertFailure "Failed to parse bigtest with schema"

    let encodeSchema = B.toStrictByteString ($(genBuilder bigTestCodec) bt)
    -- assertEqual "Encoded nbt is equal in size" (BS.length fileBS) (BS.length encodeSchema + 5) -- Top level name "level" is missing, which is fine
    NBT _ t2 <- case decodeNBT encodeSchema of
      Nothing -> assertFailure "Invalid nbt (parse from schema encoded)"
      Just x -> pure x
    assertEqual "schema == normal" t1 t2

  -- when (name == "simple_player.dat") $ error $ show nbt0

  assertEqual "Roundtrip: NBT -> Bytestring -> NBT" nbt0 nbt1

genNBT :: H.Gen NBT
genNBT = NBT <$> genNBTString <*> genTag
  where
    genNBTString = NBTString . fst <$> genModifiedUtf8 (HR.linear 0 4096) HG.unicode
    genTag = HG.recursive
      HG.choice
      genPrims
      genRecs
    genPrims =
      [ TagByte   <$> HG.int8  HR.constantBounded
      , TagShort  <$> HG.int16 HR.constantBounded
      , TagInt    <$> HG.int32 HR.constantBounded
      , TagLong   <$> HG.int64 HR.constantBounded
      , TagFloat  <$> HG.float  (HR.linearFrac (-1000) 1000) 
      , TagDouble <$> HG.double (HR.linearFrac (-1000) 1000)
      , TagString <$> genNBTString
      , TagByteArray <$> genVector (HR.linear 0 4096) (HG.int8 HR.constantBounded)
      , TagIntArray  <$> genVector (HR.linear 0 4096) (coerce <$> HG.int32 HR.constantBounded)
      , TagLongArray <$> genVector (HR.linear 0 4096) (coerce <$> HG.int64 HR.constantBounded)
      ]
    genRecs =
      [ genList
      , genCompound
      ]
    genVector range genEl = do
      xs <- HG.list range genEl
      pure $ S.fromList xs
    genList = do
      -- We cannot go through genTag here because we need one kind of tag only
      --  while using genTag gives us a random tag
      -- This also means we don't use recursive here and thus manually halve the size
      let allGens = genPrims <> fmap (HG.scale (`quot` 2)) genRecs
          numGens = length allGens - 1
      i <- HG.int (HR.constant 0 numGens)
      xs <- HG.list (HR.linear 0 100) (allGens !! i)
      let smallArrFromList [] = runST $ newSmallArray 0 (error "SmallArray empty") >>= unsafeFreezeSmallArray
          smallArrFromList xs' =
            let len = length xs'
            in runST $ do
              mar <- newSmallArray len (error "SmallArray fromList init")
              let go !_ [] = pure ()
                  go !n (y:ys) = writeSmallArray mar n y >> go (n + 1) ys
              go 0 xs'
              unsafeFreezeSmallArray mar
      pure . TagList $ smallArrFromList xs
    genCompound = do
      xs0 <- HG.list (HR.linear 0 100) genNBT
      let xs = sortOn (\(NBT k _) -> k) xs0
      pure . TagCompound $ compoundFromListAscending xs

encodeNBT :: NBT -> BS.ByteString
encodeNBT nbt = B.toStrictByteString (putNBT nbt)

decodeNBT :: BS.ByteString -> Maybe NBT
decodeNBT bs = case FP.runParser parseNBT bs of
  FP.OK res remBs | BS.null remBs -> Just res
  _ -> Nothing

readFile :: String -> IO BS.ByteString
readFile name = do
  initBs <- BS.readFile ("test/NBT/" ++ name)
  -- If the decompression throws just assume it wasn't compressed, ugly but will work
  handle (\(_ :: SomeException) -> pure initBs) . evaluate . LBS.toStrict . GZip.decompress $ LBS.fromStrict initBs 
