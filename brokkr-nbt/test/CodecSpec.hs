{-# LANGUAGE OverloadedStrings, TemplateHaskell, DataKinds #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-splices -fforce-recomp #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module CodecSpec (
  testCodec
) where

import Brokkr.NBT.Codec
import Brokkr.NBT.Internal
-- TODO Move this to internal
import Brokkr.NBT.Slice qualified as Slice

import Data.ByteString qualified as BS

import Data.Int
import Data.List (sortOn)

import FlatParse.Basic qualified as FP

import Mason.Builder qualified as B

import Test.Tasty
import Test.Tasty.HUnit

testCodec :: TestTree
testCodec = testGroup "Codec decoding" [
    testDecodeFromNBT
  ]

testDecodeFromNBT :: TestTree
testDecodeFromNBT = testCase "Decode from nbt" $ do
  let nbt0 = NBT "Hello" $ toComp [NBT "World" $ TagInt 32, NBT "Fourteen" $ TagShort 13, NBT "No" $ toComp []]
      toComp = TagCompound . Slice.fromList . sortOn (\(NBT k _) -> k)
      bs = encodeNBT nbt0
      -- Just nbt = decodeNBT bs
      -- parseCodec = $$(
      --   let co = compound "root" $ [|| (,) ||]
      --             <$#> requiredField @Int32 "World" .= [|| fst ||]
      --             <*#> requiredField @Int16 "Fourteen" .= [|| snd ||]
      --   in genNBTReader co)
      binParseCodec = $$(
        -- let co = compound "root" $ requiredField @Int32 "World2"
        let co = compound "root" $ [|| (,) ||]
                  <$#> requiredField @Int32 "World" .= [|| fst ||]
                  -- <*#> requiredField @Int16 "Fourteen" .= [|| snd ||]
                  <*#> requiredField @Int16 "Fourteen" .= [|| snd ||]
        in genParser co)
  -- case parseCodec nbt of
  --   Left err -> assertFailure $ "Failed decoding nbt. Got the following error: " <> show err
  --   Right el -> assertEqual "Read result" el (32,13)
  case FP.runParser binParseCodec bs of
    FP.OK res remBs | BS.null remBs -> assertEqual "Equal parse" (32,13) res
    _ -> assertFailure $ "Failed to parse"

encodeNBT :: NBT -> BS.ByteString
encodeNBT nbt = B.toStrictByteString (putNBT nbt)

decodeNBT :: BS.ByteString -> Maybe NBT
decodeNBT bs = case FP.runParser parseNBT bs of
  FP.OK res remBs | BS.null remBs -> Just res
  _ -> Nothing
