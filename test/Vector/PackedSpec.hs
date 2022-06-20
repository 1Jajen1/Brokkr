{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Vector.PackedSpec where

import Test.Syd
import Test.Syd.Validity

import Control.Monad.ST
import qualified Foreign as FP
import Data.Word
import Control.Monad
import Data.Bits
import qualified Util.Vector.Packed as P
import qualified Test.QuickCheck as Gen
import Control.Monad.Primitive (unsafePrimToPrim)
import qualified Test.QuickCheck as Q
import qualified Foreign.ForeignPtr.Unsafe as FP
import qualified Data.Primitive.Ptr as Prim
import GHC.TypeLits
import qualified Data.Vector.Storable as S
import Data.Semigroup
import Data.Coerce
import Data.List (nub)

spec :: Spec
spec = describe "PackedVector" $ do
  -- TODO Read and write from/to arbitrary sizes
  describe "basicRead" $ do
    specify "64: basicWrite v i a >> basicRead v i == pure a" $ forAllValid $ \(VectorWrite @64 i el (SomePackedVector v)) -> runST $ do
      mv <- P.unsafeThaw v
      P.unsafeWrite mv i el
      nEl <- P.unsafeRead mv i
      pure $ nEl Q.=== el
    specify "49: basicWrite v i a >> basicRead v i == pure a" $ forAllValid $ \(VectorWrite @49 i el (SomePackedVector v)) -> runST $ do
      mv <- P.unsafeThaw v
      P.unsafeWrite mv i el
      nEl <- P.unsafeRead mv i
      pure $ nEl Q.=== el
  describe "unsafeCopy" $ do
    specify "unsafeCopy v1 v2 => v1 == v2" $ forAllValid $ \(SomePackedVector @64 v1, SomePackedVector @64 v2) -> runST $ do
      mv1 <- P.unsafeThaw v1
      let destBSz = P.bitSz mv1
      mv2 <- P.unsafeThaw v2
      P.unsafeCopy mv1 mv2
      -- Collect both vectors and don't forget that the first one has the values from the second but truncated!
      pure $ P.foldMap (\w -> [w]) v1 Q.=== P.foldMap (\w -> [((unsafeShiftL 1 destBSz) - 1) .&. w]) v2
  specify "countElems" $ forAllValid $ \(SomePackedVector @128 v, (ints :: [Word8])) ->
    let act = P.countElems els v
        -- TODO Mask them into whatever bitsize we use
        els = S.fromList $ take 3 $ nub $ (fromIntegral . (.&. 15)) <$> ints
        exp = coerce $ P.foldMap (\x -> if S.elem x els then Sum (1 :: Int) else Sum 0) v
    in (not $ null ints) Q.==> act `shouldBe` exp

-- Generate random valid writes
data VectorWrite (sz :: Nat) = VectorWrite Int Word (SomePackedVector sz)

instance Show (VectorWrite sz) where
  show (VectorWrite i el v) = "Write " <> show i <> " " <> show el <> " with Vector " <> show v

instance KnownNat sz => GenValid (VectorWrite sz) where
  genValid = do
    v@(SomePackedVector vec) <- genValid @(SomePackedVector sz)
    let (len, bSz) =
          runST $ do
            mv <- P.unsafeThaw vec
            pure (P.length mv, P.bitSz mv)
    i <- Gen.choose (0, len)
    el <- Gen.choose (0, (unsafeShiftL 1 bSz) - 1)
    pure $ VectorWrite i el v
  shrinkValid _ = []

instance Validity (VectorWrite sz) where
  validate _ = valid

-- Generate vectors
data SomePackedVector (sz :: Nat) where
  SomePackedVector :: forall sz v . (P.PVector v, Show v) => v -> SomePackedVector sz

instance Show (SomePackedVector sz) where
  show (SomePackedVector v) = show v

instance KnownNat sz => GenValid (SomePackedVector sz) where
  genValid = Gen.oneof [
    -- First a few different statically sized vectors
      do
        xs <- Gen.vector @Word8 sz
        let wordSz = nrWords 5 sz
            wordMask = (unsafeShiftL 1 5) - 1
        pure $ runST $ unsafePrimToPrim $ do
          fptr <- FP.mallocForeignPtrArray wordSz
          let ptr = FP.unsafeForeignPtrToPtr fptr
          Prim.setPtr ptr wordSz 0
          mpv <- P.unsafeThaw $ P.unsafeStaticFromForeignPtr @sz @5 fptr
          forM_ (zip xs [0..]) $ \(el, i) -> P.unsafeWrite mpv i $ wordMask .&. fromIntegral el 
          pv <- P.unsafeFreeze mpv
          pure $ SomePackedVector pv
    , do
        xs <- Gen.vector @Word8 sz
        let wordSz = nrWords 23 sz
            wordMask = (unsafeShiftL 1 23) - 1
        pure $ runST $ unsafePrimToPrim $ do
          fptr <- FP.mallocForeignPtrArray wordSz
          let ptr = FP.unsafeForeignPtrToPtr fptr
          Prim.setPtr ptr wordSz 0
          mpv <- P.unsafeThaw $ P.unsafeStaticFromForeignPtr @sz @23 fptr
          forM_ (zip xs [0..]) $ \(el, i) -> P.unsafeWrite mpv i $ wordMask .&. fromIntegral el 
          pv <- P.unsafeFreeze mpv
          pure $ SomePackedVector pv
    , do
        xs <- Gen.vector @Word8 sz
        let wordSz = nrWords 4 sz
            wordMask = (unsafeShiftL 1 4) - 1
        pure $ runST $ unsafePrimToPrim $ do
          fptr <- FP.mallocForeignPtrArray wordSz
          let ptr = FP.unsafeForeignPtrToPtr fptr
          Prim.setPtr ptr wordSz 0
          mpv <- P.unsafeThaw $ P.unsafeStaticFromForeignPtr @sz @4 fptr
          forM_ (zip xs [0..]) $ \(el, i) -> P.unsafeWrite mpv i $ wordMask .&. fromIntegral el 
          pv <- P.unsafeFreeze mpv
          pure $ SomePackedVector pv
    -- Next some dynamic sized vectors
    , do
        bitSz <- Gen.elements [1..63]
        xs <- Gen.vector @Word sz
        let wordSz = nrWords bitSz sz
            wordMask = (unsafeShiftL 1 bitSz) - 1
        pure $ runST $ unsafePrimToPrim $ do
          fptr <- FP.mallocForeignPtrArray wordSz
          let ptr = FP.unsafeForeignPtrToPtr fptr
          Prim.setPtr ptr wordSz 0
          mpv <- P.unsafeThaw $ P.unsafeDynamicFromForeignPtr @sz bitSz fptr
          forM_ (zip xs [0..]) $ \(el, i) -> P.unsafeWrite mpv i $ wordMask .&. el 
          pv <- P.unsafeFreeze mpv
          pure $ SomePackedVector pv
    ]
    where sz = fromIntegral $ natVal @sz undefined
  shrinkValid _ = []

instance Validity (SomePackedVector sz) where
  validate _ = valid

nrWords :: Int -> Int -> Int
nrWords bSz i = (i + perWord - 1) `div` perWord
  where perWord = 64 `div` bSz
{-# INLINE nrWords #-}
