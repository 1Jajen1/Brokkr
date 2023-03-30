{-# LANGUAGE DataKinds #-}
module Generators (
  SomeVector(..)
, genFixedSize
) where

import Brokkr.PackedVector.Internal

import Test.Syd.Validity
import Test.QuickCheck.Gen
import Data.Proxy
import qualified Brokkr.PackedVector as P
import Brokkr.PackedVector.Pack
import GHC.TypeLits (KnownNat, Div)
import Data.Bits

data SomeNat = forall n . (KnownNat n, KnownNat (Div 64 n)) => SomeNat (Proxy n)

data SomeVector a = forall v . (Show (v a), PVector v a) => SV Bool Bool Int Int [a] (v a)

genFixedSize :: forall a . (Show a, Pack a, GenValid a) => Int -> Gen (SomeVector a)
genFixedSize sz = do
  bitSz <- chooseInt (1, 64)
  xs <- fmap (fmap (unpack bitSz 0 . pack bitSz 0)) $ vectorOf sz $ genValid @a
  let szT = if sz == 0 || (popCount sz /= 1 && sz > 64) then Nothing else Just $ toNat sz
      bSzT = toNat bitSz
  elements $ concat [
      case szT of
        Just (SomeNat (_ :: Proxy sz)) ->
          [ case bSzT of
              SomeNat (_ :: Proxy bSz) ->
                SV True True sz bitSz xs $ P.fromList @('Static sz) @('Static bSz) xs
          , SV True False sz bitSz xs $ P.fromList @('Static sz) @'Dynamic xs bitSz
          ]
        Nothing -> [] -- sz cannot be converted to SomeNat (I mean it can, but I don't want to expand toNat even more)
    , [ case bSzT of
          SomeNat (_ :: Proxy bSz) ->
            SV False True sz bitSz xs $ P.fromList @'Dynamic @('Static bSz) xs sz
      , SV False False sz bitSz xs $ P.fromList @'Dynamic @'Dynamic xs sz bitSz
      ]
    ]

instance Show a => Show (SomeVector a) where
  show (SV _ _ _ _ _ v) = show v

instance (Show a, Pack a, GenValid a) => GenValid (SomeVector a) where
  genValid = sized $ \s -> do
    -- Any size (bounded by 128 for sensible runtime and error output). Powers of 2 and numbers below 64 are eligble for fixed sizes to treat them special here
    sz <- frequency [(1,pure 0),(s, fmap fromIntegral $ chooseUpTo 128), (3, elements [1 `unsafeShiftL` x | x <- [0..12]])]
    genFixedSize sz

  shrinkValid (SV staticSz1 staticBsz1 sz1 bSz1 xs1 _) = do
    staticSz <- if staticSz1 then [False,True] else [False]
    staticBsz <- if staticBsz1 then [False, True] else [False]
    sz <- if staticSz then filter (\x -> x < sz1) $ [1 `unsafeShiftL` x | x <- [0..12]] else fmap fromIntegral $ shrinkValid $ fromIntegral @Int @Word sz1
    bSz <- fmap fromIntegral $ shrinkValid $ fromIntegral @Int @Word bSz1
    if bSz <= 0 then [] else pure ()
    xs <- traverse (fmap (unpack bSz 0 . pack bSz 0) . shrinkValid) $ take sz xs1
    let szT = toNat sz
        bSzT = toNat bSz
    pure $ case (staticSz, staticBsz) of
      (True, True) -> case szT of
        SomeNat (_ :: Proxy sz) -> case bSzT of
          SomeNat (_ :: Proxy bSz) -> SV True True sz bSz xs $ P.fromList @('Static sz) @('Static bSz) xs
      (True, False) -> case szT of
        SomeNat (_ :: Proxy sz) -> SV True False sz bSz xs $ P.fromList @('Static sz) @'Dynamic xs bSz
      (False, True) -> case bSzT of
        SomeNat (_ :: Proxy bSz) -> SV False True sz bSz xs $ P.fromList @'Dynamic @('Static bSz) xs sz
      (False, False) -> SV False False sz bSz xs $ P.fromList @'Dynamic @'Dynamic xs sz bSz 

instance Validity (SomeVector a) where
  validate _ = valid

toNat :: Int -> SomeNat
toNat 1 = SomeNat $ Proxy @1
toNat 2 = SomeNat $ Proxy @2
toNat 3 = SomeNat $ Proxy @3
toNat 4 = SomeNat $ Proxy @4
toNat 5 = SomeNat $ Proxy @5
toNat 6 = SomeNat $ Proxy @6
toNat 7 = SomeNat $ Proxy @7
toNat 8 = SomeNat $ Proxy @8
toNat 9 = SomeNat $ Proxy @9
toNat 10 = SomeNat $ Proxy @10
toNat 11 = SomeNat $ Proxy @11
toNat 12 = SomeNat $ Proxy @12
toNat 13 = SomeNat $ Proxy @13
toNat 14 = SomeNat $ Proxy @14
toNat 15 = SomeNat $ Proxy @15
toNat 16 = SomeNat $ Proxy @16
toNat 17 = SomeNat $ Proxy @17
toNat 18 = SomeNat $ Proxy @18
toNat 19 = SomeNat $ Proxy @19
toNat 20 = SomeNat $ Proxy @20
toNat 21 = SomeNat $ Proxy @21
toNat 22 = SomeNat $ Proxy @22
toNat 23 = SomeNat $ Proxy @23
toNat 24 = SomeNat $ Proxy @24
toNat 25 = SomeNat $ Proxy @25
toNat 26 = SomeNat $ Proxy @26
toNat 27 = SomeNat $ Proxy @27
toNat 28 = SomeNat $ Proxy @28
toNat 29 = SomeNat $ Proxy @29
toNat 30 = SomeNat $ Proxy @30
toNat 31 = SomeNat $ Proxy @31
toNat 32 = SomeNat $ Proxy @32
toNat 33 = SomeNat $ Proxy @33
toNat 34 = SomeNat $ Proxy @34
toNat 35 = SomeNat $ Proxy @35
toNat 36 = SomeNat $ Proxy @36
toNat 37 = SomeNat $ Proxy @37
toNat 38 = SomeNat $ Proxy @38
toNat 39 = SomeNat $ Proxy @39
toNat 40 = SomeNat $ Proxy @40
toNat 41 = SomeNat $ Proxy @41
toNat 42 = SomeNat $ Proxy @42
toNat 43 = SomeNat $ Proxy @43
toNat 44 = SomeNat $ Proxy @44
toNat 45 = SomeNat $ Proxy @45
toNat 46 = SomeNat $ Proxy @46
toNat 47 = SomeNat $ Proxy @47
toNat 48 = SomeNat $ Proxy @48
toNat 49 = SomeNat $ Proxy @49
toNat 50 = SomeNat $ Proxy @50
toNat 51 = SomeNat $ Proxy @51
toNat 52 = SomeNat $ Proxy @52
toNat 53 = SomeNat $ Proxy @53
toNat 54 = SomeNat $ Proxy @54
toNat 55 = SomeNat $ Proxy @55
toNat 56 = SomeNat $ Proxy @56
toNat 57 = SomeNat $ Proxy @57
toNat 58 = SomeNat $ Proxy @58
toNat 59 = SomeNat $ Proxy @59
toNat 60 = SomeNat $ Proxy @60
toNat 61 = SomeNat $ Proxy @61
toNat 62 = SomeNat $ Proxy @62
toNat 63 = SomeNat $ Proxy @63
toNat 64 = SomeNat $ Proxy @64
-- For fixed vector lengths
toNat 128 = SomeNat $ Proxy @128
toNat 256 = SomeNat $ Proxy @256
toNat 512 = SomeNat $ Proxy @512
toNat 1024 = SomeNat $ Proxy @1024
toNat 2048 = SomeNat $ Proxy @2048
toNat 4096 = SomeNat $ Proxy @4096
toNat _ = error "toNat"
