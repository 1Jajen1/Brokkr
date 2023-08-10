module VarLongSpec (
  spec
) where

import Test.Syd
import Test.Syd.Hedgehog ()

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Brokkr.VarLong

import Mason.Builder qualified as Mason
import FlatParse.Basic qualified as Flatparse

import Data.ByteString qualified as BS

spec :: Spec
spec = do
  describe "VarLong (64 bit)" $ do
    it "should parse a few examples correctly" $ do
      let encode x = Mason.toStrictByteString $ putVarLong x
          decode bs = case Flatparse.runParser (withVarLong id pure) bs of
                        Flatparse.OK res remBS | BS.null remBS -> Just res
                        _ -> Nothing
          zeroBs = BS.pack [0]
          oneBs  = BS.pack [1]
          twoFiftyBs = BS.pack [0x80, 0x01]
    
      decode zeroBs `shouldBe` Just (VarLong 0)
      encode (VarLong 0) `shouldBe` zeroBs

      decode oneBs `shouldBe` Just (VarLong 1)
      encode (VarLong 1) `shouldBe` oneBs

      decode twoFiftyBs `shouldBe` Just (VarLong 128)
      encode (VarLong 128) `shouldBe` twoFiftyBs

    it "should roundtrip" . property $ do
      let encode x = Mason.toStrictByteString $ putVarLong x
          decode bs = case Flatparse.runParser (withVarLong id pure) bs of
                        Flatparse.OK res remBS | BS.null remBS -> Just res
                        _ -> Nothing
      x <- forAll $ VarLong . fromIntegral <$> Gen.int Range.constantBounded

      tripping x encode decode
