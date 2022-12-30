{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Util.Vector.Packed (
  DynamicNat(..)
, PackedVector
, unsafeIndex
, MutablePackedVector
, Mutable
, PVector(..)
, MPVector(..)
, unsafeRead
, unsafeWrite
, unsafeCopy
, foldMap
, foldl'
, countElems
, unsafeStaticFromForeignPtr
, unsafeDynamicFromForeignPtr
, nrWords
) where

import qualified Prelude
import Prelude hiding (foldMap, length )

import Control.Monad.Primitive
import Control.Monad.ST ( runST )

import Data.Bits
import qualified Data.Primitive.Ptr as P
import qualified Data.ByteString.Internal as B

import qualified Foreign.ForeignPtr as FP
import qualified Foreign.Storable as S
import qualified Foreign.Marshal.Utils as S
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Primitive as Prim
import qualified Data.Primitive as Prim

import GHC.ForeignPtr ( unsafeWithForeignPtr )
import GHC.TypeLits ( KnownNat, Nat, natVal )
import GHC.Word ( Word64(W64#) )
import qualified Data.Primitive.Ptr as Ptr
import Control.DeepSeq
import GHC.Base

import Util.Vector.Internal
import qualified Data.Foldable
import Util.NBT.Internal
import Util.Binary

import qualified Mason.Builder.Internal as Mason
import Foreign.Ptr (Ptr, plusPtr)
import Data.Word

-- TODO Check if it is worth avoiding the overflow/underflow checks that div imposes
--  Unless vectors with a packed bitSize of 0 or > 64 are introduced, which are nonsense, this should never fail
-- TODO Tests + Benchmarks (Compare against normal storable vectors to get a sense of what overhead we have)
-- TODO Make data instances unlifted and newtype instances rep poly?

-- | Either some 'Nat' value of a dynamic (usually runtime defined) value
data DynamicNat = Static Nat | Dynamic

-- | Bit-packed vector using 64bit unsigned integers
--
-- Both the size (in packed values) and the bit-size of our packed unsigned integers can be defined as
-- either statically known types or dynamic runtime values
--
-- There is currently no instances for dynamically sized packed vectors as I have not needed them yet.
data family PackedVector (sz :: DynamicNat) (bSz :: DynamicNat)

-- A vector whose size and bitSize properties are both statically known can be represented as just
-- a 'ForeignPtr' of unsigned 64 bit words.
newtype instance PackedVector ('Static sz) ('Static bSz) = PV_Stat (FP.ForeignPtr Word64)

-- A vector with a dynamic bitSize has to carry around its bitSize alongside its data.
data instance PackedVector ('Static sz) 'Dynamic = PV_Dyn {-# UNPACK #-} !Int {-# UNPACK #-} !(FP.ForeignPtr Word64)

-- | /O(1)/ Index into a packed vector
--
-- Returns a 'Word', but at most n bits (with n being the packed words bit size) are set.
--
-- Does no bounds checking.
unsafeIndex :: forall a . PVector a => a -> Int -> Word
unsafeIndex !arr !i = runST $ unsafeThaw arr >>= flip unsafeRead i
{-# INLINE unsafeIndex #-}

-- | Bit-packed mutable vector using 64bit unsigned integers
data family MutablePackedVector (sz :: DynamicNat) (bSz :: DynamicNat) s

newtype instance MutablePackedVector ('Static sz) ('Static bSz) s = MPV_Stat (FP.ForeignPtr Word64)

data instance MutablePackedVector ('Static sz) 'Dynamic s = MPV_Dyn {-# UNPACK #-} !Int {-# UNPACK #-} !(FP.ForeignPtr Word64)

-- | Map immutable packed vectors to mutable packed vectors
type family Mutable (v :: Type) = (mv :: Type -> Type) | mv -> v
type instance Mutable (PackedVector sz bSz) = MutablePackedVector sz bSz

-- | Generic immutable packed vectors
-- TODO all methods from MPVector should be copied here
class MPVector (Mutable a) => PVector a where
  -- | /O(1)/ Turn an immutable packed vector into a mutable vector without copying
  unsafeThaw :: PrimMonad m => a -> m (Mutable a (PrimState m))
  -- | /O(1)/ Turn a mutable packed vector into an immutable vector without copying
  unsafeFreeze :: PrimMonad m => Mutable a (PrimState m) -> m a

-- | Generic mutable packed vectors
class MPVector (a :: Type -> Type) where
  -- | /O(1)/ Returns the length in packed elements
  length :: a s -> Int
  -- | /O(1)/ Returns the size in bits of the packed elements
  bitSz :: a s -> Int
  -- | /O(1)/ Returns the backing 'ForeignPtr'
  backing :: a s -> FP.ForeignPtr Word64

instance (KnownNat sz, KnownNat bSz) => PVector (PackedVector ('Static sz) ('Static bSz)) where
  unsafeThaw = pure . coerce
  {-# INLINE unsafeThaw #-}
  unsafeFreeze = pure . coerce
  {-# INLINE unsafeFreeze #-}

instance (KnownNat sz, KnownNat bSz) => MPVector (MutablePackedVector ('Static sz) ('Static bSz)) where
  length _ = fromIntegral $ natVal @sz undefined
  {-# INLINE length #-}
  bitSz _ = fromIntegral $ natVal @bSz undefined
  {-# INLINE bitSz #-}
  backing = coerce
  {-# INLINE backing #-}

instance KnownNat sz => PVector (PackedVector ('Static sz) 'Dynamic) where
  unsafeThaw (PV_Dyn bs fptr) = pure $ MPV_Dyn bs fptr
  {-# INLINE unsafeThaw #-}
  unsafeFreeze (MPV_Dyn bs fptr) = pure $ PV_Dyn bs fptr
  {-# INLINE unsafeFreeze #-}

instance KnownNat sz => MPVector (MutablePackedVector ('Static sz) 'Dynamic) where
  length _ = fromIntegral $ natVal @sz undefined
  {-# INLINE length #-}
  bitSz (MPV_Dyn bs _) = bs
  {-# INLINE bitSz #-}
  backing (MPV_Dyn _ fptr) = fptr
  {-# INLINE backing #-}

instance PVector (PackedVector sz bSz) => Show (PackedVector sz bSz) where
  show v = "PackedVector " <> show len <> " " <> show bSz <> " " <>  (show $ foldMap (\a -> [a]) v)
    where (len, bSz) = runST $ do
            mv <- unsafeThaw v
            pure $ (length mv, bitSz mv)

instance NFData (PackedVector ('Static sz) ('Static bSz)) where
  rnf (PV_Stat _) = ()

instance NFData (PackedVector ('Static sz) 'Dynamic) where
  rnf (PV_Dyn len _) = rnf len

instance FromNBT (PackedVector ('Static sz) ('Static bSz)) where
  parseNBT (TagLongArray arr) = pure . unsafeStaticFromForeignPtr . coerce . fst $ S.unsafeToForeignPtr0 arr
  parseNBT (TagByteArray arr) = pure . unsafeStaticFromForeignPtr . coerce . fst $ S.unsafeToForeignPtr0 arr
  parseNBT (TagIntArray arr)  = pure . unsafeStaticFromForeignPtr . coerce . fst $ S.unsafeToForeignPtr0 arr
  parseNBT _ = error "Not a packed vector at all!"

instance (KnownNat sz, KnownNat bSz) => ToNBT (PackedVector ('Static sz) ('Static bSz)) where
  toNBT pv@(PV_Stat fptr) = TagLongArray $ S.unsafeFromForeignPtr0 (coerce fptr) sz
    where
      !vLen = runST $ length <$> unsafeThaw pv
      !vBSz = runST $ bitSz <$> unsafeThaw pv
      !sz = nrWords vBSz vLen

instance Eq (PackedVector ('Static _sz) ('Static _bSz)) where
  PV_Stat f1 == PV_Stat f2 = f1 == f2

instance Eq (PackedVector ('Static _sz) 'Dynamic) where
  PV_Dyn l1 f1 == PV_Dyn l2 f2 = l1 == l2 && f1 == f2

instance (KnownNat sz, KnownNat bSz) => ToBinary (PackedVector ('Static sz) ('Static bSz)) where
  put pv@(PV_Stat fptr) = Mason.withPtr nrBytes $ \ptr -> unsafeWithForeignPtr fptr $ \src -> do
    B.memcpy ptr (coerce src) nrBytes
    pure $ ptr `plusPtr` nrBytes
    where
      vLen = runST $ length <$> unsafeThaw pv
      vBSz = runST $ bitSz <$> unsafeThaw pv
      nrBytes = 8 * (nrWords vBSz vLen)

instance KnownNat sz => ToBinary (PackedVector ('Static sz) 'Dynamic) where
  put pv@(PV_Dyn bSz fptr) = Mason.withPtr nrBytes $ \ptr -> unsafeWithForeignPtr fptr $ \src -> do
    B.memcpy ptr (coerce src) nrBytes
    pure $ ptr `plusPtr` nrBytes
    where
      vLen = runST $ length <$> unsafeThaw pv
      nrBytes = 8 * (nrWords bSz vLen)

-- Methods

-- | /O(1)/ Read a packed element from a packed vector at a given index
--
-- Returns a 'Word', but at most n bits (with n being the packed words bit size) are set.
--
-- Does no bounds checking.
unsafeRead :: (MPVector a, PrimMonad m) => a (PrimState m) -> Int -> m Word
unsafeRead vec !i =
  let bs      = bitSz vec
      fptr    = backing vec
      wordInd = divSize bs i
      aInd    = modSize bs i
  in unsafePrimToPrim $ indexPacked bs aInd <$> unsafeWithForeignPtr fptr (flip S.peekElemOff wordInd)
{-# INLINE unsafeRead #-}

-- | /O(1)/ Write a packed element to a packed vector at a given index
--
-- Does no bounds checking. Also does not mask the given 'Word', this means
-- if a bit higher than the packed element's BitSize is set, it may overflow
-- into the next word.
unsafeWrite :: (MPVector a, PrimMonad m) => a (PrimState m) -> Int -> Word -> m ()
unsafeWrite vec !i !a =
  let bs      = bitSz vec
      fptr    = backing vec
      wordInd = divSize bs i
      aInd    = modSize bs i
  in unsafePrimToPrim $ unsafeWithForeignPtr fptr $ \ptr ->
    S.peekElemOff ptr wordInd >>= S.pokeElemOff ptr wordInd . writePacked bs aInd a
{-# INLINE unsafeWrite #-}

-- | /O(min(srcBitSize, dstBitSize) \* n \/ 64)/ Copy into packed vector from another packed vector
-- of equal element length but not necessarily of equal bitsize.
--
-- The two vectors can have different bitsize's for their elements, but the total length in
-- terms of packed elements has to be the same. This is not checked.
-- 
-- With @0 < min(srcBitSize, dstBitSize) <= 64@ we can assert that the time complexity is always
-- smaller than a naive implementation reading and writing each packed word individually.

-- TODO Rebench and optimize traversals, this should not really be worth it
unsafeCopy :: (MPVector a, MPVector b, PrimMonad m) => a (PrimState m) -> b (PrimState m) -> m ()
-- The naive implementation here would be to read every word one by one and write it to the destination so O(n)
-- 
-- On the destBitSize > srcBitSize branch we can cut this down to O(n * destBitSize / 64) using pdep64
-- Similarly the destBitSize < srcBitSize branch gets O(n * srcBitSize / 64) using pext64
--
-- pext/pdep implement gather/scatter operations for bits in a word:
--
-- Example for pext using 8 bit numbers:
-- input  b7 b6 b5 b4 b3 b2 b1
-- mask   1  0  1  0  1  0  1
-- result 0  0  0  b7 b5 b3 b1  "pext collects every value marked with a 1 in one contigous segment"
--
-- Example for pdep:
-- input  b7 b6 b5 b4 b3 b2 b1
-- mask   1  1  1  0  1  0  0
-- result b4 b3 b2 0  b1 0  0   "pdep puts the values (from first to last) to each spot marked with a 1"
--
-- Example for 3 -> 5 copy:
-- Since we are writing into a larger bitsize we need to scatter our bits, so pdep is used:
--
-- input  c2 c1 c0 b2 b1 b0 a2 a1 a0                       (3 3 bit words)
-- mask   0  0  1  1  1  0  0  1  1  1  0  0  1  1  1      (0 0 1 1 1 repeated 3 times)
-- result 0  0  c2 c1 c0 0  0  b2 b1 b0 0  0  a2 a1 a0     (3 5 bit words with the lowest 3 bit from our old data)
-- 
-- With every step we can now write up to 64 / 5 = 12 values, but our previous data had 64 / 3 = 21 values. This means
--  we have to carry over some values to the next word. With some bit shifting we then combine this with the values from the next read word
--  and write that. The next step we'll have even more overflow until our overflow can fill an entire word, at which point we don't read a word
--  but write our overflow directly. This repeats until we have copied the entire vector.
--
-- Example for 5 -> 3 copy:
-- Now we are writing into a smaller bitsize so we need to gather our bits, so we use pext:
--
-- input  c4 c3 c2 c1 c0 b4 b3 b2 b1 b0 a4 a3 a2 a1 a0     (3 5 bit words)
-- mask   0  0  1  1  1  0  0  1  1  1  0  0  1  1  1      (0 0 1 1 1 repeated 3 times)
-- result 0  0  0  0  0  0  c2 c1 c0 b2 b1 b0 a2 a1 a0     (3 3 bit words with the lowest 3 bit from our old data and the higher bits lost)
--
-- Unlike copying into a larger bitsize, we now end up with an underfilled value. Thus we accumulate the value we want to write and write it
-- whenever it fills up.
unsafeCopy !dst !src
  -- TODO After writing tests I had to adjust a few things, go over this again and try to simplify
  -- TODO Split the implementation from the top level case so that we can inline those but keep inlining impl up to ghc
  -- TODO Revise uses of divInt var. Maybe write a wrapper around divInt# that does no safety checks but still does optimizations for powers of 2
  --  Same for mod.
  | dBs == sBs = unsafePrimToPrim $ unsafeWithForeignPtr dFptr $ \dPtr -> unsafeWithForeignPtr sFptr $ \sPtr ->
    S.copyBytes dPtr sPtr (8 * nrWords dBs len)
  | dBs > sBs = do
    let !(W64# pMask) = pextMask sBs dBs
        !wLen = nrWords dBs len
        !minOverflow = prevPerWord - perWord
        !overflowWordMask = (unsafeShiftL 1 (perWord * sBs)) - 1
        getOverflow 0 _ = 0
        getOverflow !nr !w = overflowWordMask .&. (unsafeShiftR w $ (prevPerWord - nr) * sBs)
        do_copy_1 !dPtr !sPtr !dN !currentWord !off
          | dN < wLen =
            -- TODO Can we simplify this to a condition later in the loop like for the dBs < sBs case?
            -- Would make both the code and its core more readable
            if | off >= perWord -> do
                  let !(W64# over#) = getOverflow off currentWord
                      nW = W64# (pdep64# over# pMask)
                  S.poke dPtr nW
                  do_copy_1 (P.advancePtr dPtr 1) sPtr (dN + 1) currentWord (off - perWord)
               | otherwise -> do
                  w <- S.peek sPtr
                  let !over = getOverflow off currentWord 
                      !(W64# w#) = over .|. (unsafeShiftL w $ off * sBs)
                      !nW = W64# (pdep64# w# pMask)
                      !nOff = minOverflow + off
                  S.poke dPtr nW
                  do_copy_1 (P.advancePtr dPtr 1) (P.advancePtr sPtr 1) (dN + 1) w nOff
          | otherwise = pure ()
    unsafePrimToPrim $ unsafeWithForeignPtr dFptr $ \dPtr -> unsafeWithForeignPtr sFptr $ \sPtr -> do_copy_1 dPtr sPtr 0 0 0
  | otherwise = do
    let !(W64# pMask) = pextMask dBs sBs
        srcLen = nrWords sBs len
        do_copy_2 !dPtr !sPtr !dN !currentWord !off
          | dN < srcLen = do
              !(W64# w) <- S.peek sPtr
              let w' = W64# (pext64# w pMask)
                  nW = currentWord .|. (unsafeShiftL w' $ off * dBs)
                  nOff = off + prevPerWord
              if nOff >= perWord
                then do
                  S.poke dPtr nW
                  let nW' = unsafeShiftR w' $ offDiff * dBs
                      offDiff = perWord - off
                  do_copy_2 (P.advancePtr dPtr 1) (P.advancePtr sPtr 1) (dN + 1) nW' (prevPerWord - offDiff)
                else do_copy_2 dPtr (P.advancePtr sPtr 1) (dN + 1) nW nOff
          | off == 0 = pure ()
          | otherwise = S.poke dPtr currentWord
    unsafePrimToPrim $ unsafeWithForeignPtr dFptr $ \dPtr -> unsafeWithForeignPtr sFptr $ \sPtr -> do_copy_2 dPtr sPtr 0 0 0
  where
    !prevPerWord = 64 `divInt` sBs
    !perWord = 64 `divInt` dBs
    !len = length dst
    !dFptr = backing dst
    !sFptr = backing src
    !dBs = bitSz dst
    !sBs = bitSz src
{-# INLINE unsafeCopy #-}

-- | O(n) Fold over the content of a packed vector
foldMap :: (PVector v, Monoid m) => (Word -> m) -> v -> m
foldMap f v = runST $ do
  mv <- unsafeThaw v
  let len = Util.Vector.Packed.length mv
      bSz = bitSz mv
      fptr = backing mv
      wLen = nrWords bSz len
      perWord = 64 `divInt` bSz
      go ptr n x acc
        | n < wLen = do
          w <- S.peek ptr
          let nAcc = acc <> (Prelude.foldMap (\i -> f . fromIntegral . (.&. ((unsafeShiftL 1 bSz) - 1)) $ unsafeShiftR w $ i * bSz) [0..((min perWord (len - x))) - 1])
          go (Ptr.advancePtr ptr 1) (n + 1) (x + perWord) nAcc
        | otherwise = pure acc
  unsafePrimToPrim $ unsafeWithForeignPtr fptr $ \ptr -> go ptr 0 0 mempty
{-# INLINE foldMap #-}

foldl' :: PVector v => (a -> Int -> Word -> a) -> a -> v -> a
foldl' f a v = runST $ do
  mv <- unsafeThaw v
  let len = length mv
      bSz = bitSz mv
      fptr = backing mv
      wLen = nrWords bSz len
      perWord = 64 `divInt` bSz
      go ptr n x acc
        | n < wLen = do
          w <- S.peek ptr
          let nAcc = Data.Foldable.foldl' (\a' i -> f a' (i + n * perWord) . fromIntegral . (.&. ((unsafeShiftL 1 bSz) - 1)) $ unsafeShiftR w $ i * bSz) acc [0..((min perWord (len - x))) - 1]
          go (Ptr.advancePtr ptr 1) (n + 1) (x + perWord) nAcc
        | otherwise = pure acc
  unsafePrimToPrim $ unsafeWithForeignPtr fptr $ \ptr -> go ptr 0 0 a
{-# INLINE foldl' #-}

-- Pass an Unboxed vector instead. Reason being: This uses an unsafe ffi call, so unboxed vectors are fine and
-- don't carry as much penalty for allocation and fragmentation
countElems :: PVector v => Prim.Vector Word -> v -> Int
countElems (Prim.Vector off sz arr) v =
  runST $ do
    mv <- unsafeThaw v
    let fptr = backing mv
        len = length mv
        wLen = nrWords bSz len
        bSz = bitSz mv
        ePtr = Ptr.advancePtr @Word (coerce $ Prim.byteArrayContents arr) off
    unsafePrimToPrim $ unsafeWithForeignPtr fptr $ \ptr -> do
      pure . fromIntegral $
        c_countElems (fromIntegral bSz) (fromIntegral len) (coerce ptr) (fromIntegral wLen) ePtr (fromIntegral sz)
{-# INLINE countElems #-}

-- Creating packed vectors

-- | Cast a 'ForeignPtr' of unsigned 64bit words to a packed vector
--
-- The 'ForeignPtr' is expected to contain enough 64bit words to hold 'sz' packed values
-- with an individual size of 'bSz'. This is not checked.
unsafeStaticFromForeignPtr :: FP.ForeignPtr Word64 -> PackedVector ('Static sz) ('Static bSz)
unsafeStaticFromForeignPtr = coerce
{-# INLINE unsafeStaticFromForeignPtr #-}

-- | Create a packed vector of a given dynamic bit-size and a 'ForeignPtr' of unsigned 64bit words
--
-- The 'ForeignPtr' is expected to contain enough 64bit words to hold 'sz' packed values
-- with an individual size of 'bSz'. This is not checked.
unsafeDynamicFromForeignPtr :: Int -> FP.ForeignPtr Word64 -> PackedVector ('Static sz) 'Dynamic
unsafeDynamicFromForeignPtr bSz fptr = PV_Dyn bSz fptr
{-# INLINE unsafeDynamicFromForeignPtr #-}

-- Internal Utils

-- TODO We can speed up divisions by instead multiplying the numbers in such a way that we can then divide by a power of 2
-- Lots of magic constants will get involved but that's fine, we can generate lookup tables for those
divSize :: Int -> Int -> Int
divSize bSz i = i `divInt` (64 `divInt` bSz)
{-# INLINE divSize #-}

modSize :: Int -> Int -> Int
modSize bSz i = i `modInt` (64 `divInt` bSz)
{-# INLINE modSize #-}

indexPacked :: Int -> Int -> Word64 -> Word
indexPacked bSz i w = fromIntegral . (.&. ((unsafeShiftL 1 bSz) - 1)) $ unsafeShiftR w $ i * bSz
{-# INLINE indexPacked #-}

writePacked :: Int -> Int -> Word -> Word64 -> Word64
writePacked bSz i el w = (w .&. mask) .|. mask
  where
    mask = (unsafeShiftL (fromIntegral el) (i * bSz)) .&. complement zeroBits
{-# INLINE writePacked #-}

nrWords :: Int -> Int -> Int
nrWords bSz i = (i + perWord - 1) `divInt` perWord
  where perWord = 64 `divInt` bSz
{-# INLINE nrWords #-}

-- TODO This assumes Word is Word64, which is fine since our parser does so as well
foreign import ccall unsafe "countElems" c_countElems :: Word8 -> Word32 -> Ptr Word8 -> Word32 -> Ptr Word -> Word32 -> Word32
