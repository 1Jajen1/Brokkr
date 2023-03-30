{-# LANGUAGE LambdaCase #-}
module ReadWriteSpec where

import Test.Syd
import Test.Syd.Validity

import Generators
import Test.QuickCheck
import qualified Brokkr.PackedVector as P
import Test.QuickCheck.Exception
import Brokkr.PackedVector.Pack
import qualified Brokkr.PackedVector.Mutable as MP
import Control.Monad.ST.Strict

spec :: Spec
spec = do
  describe "PackedVector" $ do
    specify "null" $ forAllValid $ \(SV @Int ss sd sz _ xs v) ->
      checkCoverage $ counterexample (show xs) $
        coverKind Nothing ss sd $
        cover 5 (sz /= 0) "not null" $
        cover 1 (sz == 0) "null" $ (sz == 0) === P.null v
    specify "(!)" $ forAllValid $ \(SV @Int ss sd sz _ xs v) ->
      checkCoverage $ counterexample (show xs) $ forAll (ranged sz) $ \ind ->
        coverKind Nothing ss sd $
        cover 75 (inRange ind sz) "in range" $
        cover  5 (not $ inRange ind sz) "not in range" $
        counterexample ("Failed at index " <> show ind) $
          if inRange ind sz
            then xs !! ind === v P.! ind
            else ioProperty $ (tryEvaluate $ v P.! ind) >>= \case
              Left _ -> pure $ property True
              Right _ -> pure $ counterexample "Expected failure" False
    specify "(!?)" $ forAllValid $ \(SV @Int ss sd sz _ xs v) ->
      checkCoverage $ counterexample (show xs) $ forAll (ranged sz) $ \ind ->
        coverKind Nothing ss sd $
        cover 75 (inRange ind sz) "in range" $
        cover  5 (not $ inRange ind sz) "not in range" $
        counterexample ("Failed at index " <> show ind) $
          if inRange ind sz
            then Just (xs !! ind) === v P.!? ind
            else Nothing === v P.!? ind
    specify "unsafeIndex" $ forAllValid $ \(SV @Int ss sd sz _ xs v) ->
      checkCoverage $ counterexample (show xs) $ forAll (ranged sz) $ \ind ->
        coverKind Nothing ss sd $
        counterexample ("Failed at index " <> show ind) $
          if inRange ind sz
            then xs !! ind === P.unsafeIndex v ind
            else discard
    specify "toList" $ forAllValid $ \(SV @Int ss sd _ _ xs v) ->
      checkCoverage $
        coverKind Nothing ss sd $
        counterexample (show xs) $ xs === P.toList v
    specify "fmap mask . toList . copy w v == toList v" $
      forAll genValid           $ \(SV @Int ss1 sd1 sz1 bSz1 _   v1) ->
      forAll (genFixedSize sz1) $ \(SV @Int ss2 sd2 _   bSz2 xs2 v2) ->
        let new = runST $ do
              dst <- P.thaw v1
              src <- P.unsafeThaw v2
              MP.copy dst src
              P.unsafeFreeze dst
            mask = unpack bSz1 0 . pack bSz1 0
        in  cover 1 (bSz1 == bSz2) "Same bit-size" $ -- TODO Checking coverage here is slow
            coverTable "Vector kinds" (do
              let opts = ["SS", "SD", "DS", "DD"]
              l <- opts
              r <- opts
              pure $ (l <> "-" <> r, 1)
            ) $
            tabulate "Vector kinds" [let l = toKind ss1 sd1; r = toKind ss2 sd2 in l <> "-" <> r] $
            P.toList new === fmap mask xs2

inRange :: Int -> Int -> Bool
inRange i upper = i >= 0 && i < upper

ranged :: Int -> Gen Int
ranged upper = sized $ \sz -> frequency [(2, chooseAny), (1 + sz, choose (0, upper - 1))]

coverKind :: Testable prop => Maybe String -> Bool -> Bool -> prop -> Property
coverKind ident staticSz staticBSz p =
  coverTable ("Vector kind" <> maybe "" (\s -> " " <> s) ident) [("SS", 1), ("SD", 1), ("DS", 5), ("DD", 5)] $ tabulate "Vector kind" [toKind staticSz staticBSz] p

toKind :: Bool -> Bool -> String
toKind True True = "SS"
toKind True False = "SD"
toKind False True = "DS"
toKind False False = "DD"
