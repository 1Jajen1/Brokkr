{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans -Wno-missing-export-lists #-}
module Util.NBTSpec where

import Test.Syd
import Test.Syd.Validity

import Data.Validity
import Data.GenValidity.Text
import Data.GenValidity.Vector
import Data.GenValidity.Map

import Test.QuickCheck

import qualified Mason.Builder as B
import qualified FlatParse.Basic as FP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.GZip as GZip

import Util.Binary
import Util.NBT.Internal

import qualified Data.Map
import qualified Data.Vector as V
import Data.Int

import Debug.Trace
import System.Directory
import Data.Text hiding (all)

spec :: Spec
spec = do
  describe "ByteString <-> NBT" $ do
    it "should parse a big nbt file" $ do
      bs <- LBS.toStrict . GZip.decompress . LBS.fromStrict <$> BS.readFile "test/Util/NBT/bigtest.nbt"
      case FP.runParser (get @NBT) bs of
        -- Same size assertion since order may change so eq is not reliable
        FP.OK nbt "" -> BS.length (B.toStrictByteString (put nbt)) `shouldBe` (BS.length bs)
        _ -> expectationFailure @() "Parsing failed"
    specify "roundtrips" $ forAllValid @NBT $ \nbt ->
      let bs = B.toStrictByteString $ put nbt
      in case FP.runParser (get @NBT) bs of
        FP.OK res "" -> res === nbt
        _ -> counterexample "Parser failed" False
  describe "NBTParser" $ do
    describe "withCompound" $ do
      it "should only succeed the inner function on compounds" $ forAllValid @(Tag, Int) $ \(tag, i) ->
        case runParser (withCompound (const $ pure i) tag) Just Nothing tag of
          Just x -> counterexample "Parser succeeded on a non-compound tag" ((tagId tag) === (tagId $ compound mempty)) .&&. counterexample "Parser returned the wrong value" (x === i)
          Nothing -> counterexample "Parser failed on a compound tag" $ (tagId tag) =/= (tagId $ compound mempty)
    describe "(.:)" $ do
      it "should produce a correct parser for an existing element" $ do
        let m = Data.Map.fromList [("Hello", toNBT @Int8 10)]
            correct = do
              m .: "Hello"
        runParser correct Just Nothing (TagCompound m) `shouldBe` (Just (10 :: Int8))
      it "should produce a correct parser for an existing element" $ do
        let m = Data.Map.fromList [("Hello", toNBT @Int8 10)]
            wrong = do
              m .: "World"
        runParser wrong Just Nothing (TagCompound m) `shouldBe` (Nothing :: Maybe Int8)
    describe "(.:?)" $ do
      it "should produce a correct parser for an existing element" $ do
        let m = Data.Map.fromList [("Hello", toNBT @Int8 10)]
            correct = do
              m .:? "Hello"
        runParser correct Just Nothing (TagCompound m) `shouldBe` (Just (Just (10 :: Int8)))
      it "should produce a correct parser for an existing element" $ do
        let m = Data.Map.fromList [("Hello", toNBT @Int8 10)]
            wrong = do
              m .:? "World"
        runParser wrong Just Nothing (TagCompound m) `shouldBe` (Just (Nothing :: Maybe Int8)) 
    describe "(.!=)" $ do
      it "should produce a correct parser for an existing element" $ do
        let m = Data.Map.fromList [("Hello", toNBT @Int8 10)]
            correct = do
              m .:? "Hello" .!= 1
        runParser correct Just Nothing (TagCompound m) `shouldBe` (Just (10 :: Int8))
      it "should produce a correct parser for an existing element" $ do
        let m = Data.Map.fromList [("Hello", toNBT @Int8 10)]
            wrong = do
              m .:? "World" .!= 1
        runParser wrong Just Nothing (TagCompound m) `shouldBe` (Just (1 :: Int8))
    describe "compound" $ do
      it "should produce the correct tag" . forAllValid @[(Text, Tag)] $ \xs ->
        let exp = TagCompound $ Data.Map.fromList xs
            act = compound xs
        in act `shouldBe` exp
    describe "(.=)" $ do
      it "should produce the correct tuple" $ forAllValid @(Text, Tag) $ \(key, val) ->
        (key .= val) `shouldBe` (key, val)
  describe "NBT" $ do
    eqSpec @NBT
  describe "Tag" $ do
    eqSpec @Tag

instance GenValid NBT where
instance GenValid Tag where

instance Validity Tag where
  validate = \case
    TagFloat f | isNaN f -> invalid "NaN messes up Eq instances"
    TagDouble d | isNaN d -> invalid "NaN messes up Eq instances"
    _ -> valid
instance Validity NBT where
  validate (NBT _ t) = case t of
    TagEnd -> invalid "TagEnd cannot be used in top-level NBT's"
    TagList xs -> if V.length xs > 0
      then let tid = tagId $ xs V.! 0
           in declare "Tag should not be TagEnd" (tid /= 0) <> declare "All list elements should have the same type" (all (\x -> tagId x == tid) xs)
      else valid
    TagCompound xs -> Data.Map.foldMapWithKey (\k tag -> validate $ NBT k tag) xs
    _ -> valid
