{-# LANGUAGE TemplateHaskell #-}
module Util.Vector.Internal (
  pextMask
) where

import Data.Word
import Data.Bits
import GHC.Base
import Language.Haskell.TH

-- This generates all masks for combinations [1..63]:[1..63] at compile time
$(do
  let pextMask' :: Int -> Int -> Word64
      pextMask' !i !j = go 0 section
        where
          section = (unsafeShiftL 1 i) - 1
          len = 64 `divInt` j
          go !n !acc | n < len = go (n + 1) (acc .|. unsafeShiftL section (n * j))
                     | otherwise = acc
  let n = mkName "pextMask"
  funD <- funD n $ do
        (i, j) :: (Int, Int) <- [(i, j) | i <- [1..63], j <- [1..63], j /= i]
        pure $ clause
          [litP $ IntegerL $ fromIntegral i, litP $ IntegerL $ fromIntegral j]
          (normalB $ litE $ IntegerL $ fromIntegral $ pextMask' i j)
          []
  funSig <- sigD n $ [t| Int -> Int -> Word64 |]
  --let inlineD = PragmaD $ InlineP n Inline ConLike AllPhases
  pure $ [funSig, funD]
  )
