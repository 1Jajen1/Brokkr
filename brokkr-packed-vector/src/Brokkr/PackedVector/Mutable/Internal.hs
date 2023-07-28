{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Brokkr.PackedVector.Mutable.Internal (
  DynamicNat(..)
, MutablePackedVector(..)
, MPVector(..)
-- Creation
, new
, unsafeFromForeignPtr
-- length information
, null
-- access to individual elements
, unsafeRead, read
, unsafeWrite, write
, unsafeModify, modify
, unsafeModifyM, modifyM
, unsafeSwap, swap
, unsafeExchange, exchange
-- folds
, ifoldM, foldM
, itraverse_, traverse_
, ifoldl', foldl'
-- copy
, unsafeCopy, copy
) where

import GHC.TypeLits
import Data.Kind
import Control.Monad (void)
import Control.Monad.Primitive
import Foreign.Ptr
import Data.Word
import Foreign.ForeignPtr (ForeignPtr)
import Data.Proxy
import qualified Brokkr.PackedVector.Util as Util
import GHC.ForeignPtr (unsafeWithForeignPtr, mallocPlainForeignPtrBytes)
import qualified Foreign.Storable as S
import Brokkr.PackedVector.Pack
import Prelude hiding (null, read, length)
import GHC.Base (quotInt)
import Foreign (copyBytes)
import Unsafe.Coerce
import GHC.IO.Unsafe (unsafePerformIO)
import qualified Foreign.Marshal.Utils as Ptr
import Control.DeepSeq
import Data.Bits (unsafeShiftR)
import Data.Coerce (coerce)

-- | Datakind to either define a statically known compile time natural number or a dynamic runtime number
-- Used by both 'PackedVector' and 'MutablePackedVector' to provide an optimized structure for each case. 
data DynamicNat = Dynamic | Static Nat

-- | Mutable packed vector. Stores elements which require less than 64 bits efficiently.
--
-- Elements are stored in 64 bit words, this means @ 64 % bitSize @ bits will be unused in each 64 bit word.
-- Each combination of 'vecSize' and 'bitSize' will yield a type with minimal runtime information required for
-- efficient use.
data family MutablePackedVector (vecSize :: DynamicNat) (bitSize :: DynamicNat) (s :: Type) (a :: Type)

newtype instance MutablePackedVector ('Static len) ('Static bitSize) (s :: Type) (a :: Type) = MPVec_SS (ForeignPtr Word64) 
  deriving newtype Eq

data instance MutablePackedVector 'Dynamic ('Static bSz) (s :: Type) (a :: Type) =
    MPVec_DS
      {-# UNPACK #-} !Int -- size in elements
      {-# UNPACK #-} !(ForeignPtr Word64)
  deriving stock Eq

data instance MutablePackedVector ('Static len) 'Dynamic (s :: Type) (a :: Type) =
    MPVec_SD
      {-# UNPACK #-} !Int -- Element bit size
      -- See note [Divison by "almost" constants]
      {-# UNPACK #-} !Int -- Elements per word
      {-# UNPACK #-} !Int -- Mult. constant for 1/ (64 / bitSize)
      {-# UNPACK #-} !Int -- Add. constant ^
      {-# UNPACK #-} !Int -- Bitshift constant
      {-# UNPACK #-} !(ForeignPtr Word64)
  deriving stock Eq

data instance MutablePackedVector 'Dynamic 'Dynamic (s :: Type) (a :: Type) =
    MPVec_DD 
      {-# UNPACK #-} !Int -- Size in elements
      {-# UNPACK #-} !Int -- Element bit size
      -- See note [Divison by "almost" constants]
      {-# UNPACK #-} !Int -- Elements per word
      {-# UNPACK #-} !Int -- Mult. constant for 1/ (64 / bitSize)
      {-# UNPACK #-} !Int -- Add. constant ^
      {-# UNPACK #-} !Int -- Bitshift constant
      {-# UNPACK #-} !(ForeignPtr Word64)
  deriving stock Eq

instance NFData (MutablePackedVector ('Static sz) ('Static bSz) s a) where
  rnf (MPVec_SS _) = ()
instance NFData (MutablePackedVector ('Static sz) 'Dynamic s a) where
  rnf MPVec_SD {} = ()
instance NFData (MutablePackedVector 'Dynamic ('Static bSz) s a) where
  rnf (MPVec_DS _ _) = ()
instance NFData (MutablePackedVector 'Dynamic 'Dynamic s a) where
  rnf MPVec_DD {} = ()

{- Note: Mutable and immutable variants

Both the immutable and mutable variants have the exact same definition except that the mutable variant has
an additional type parameter for the state thread. This allows somewhat safe use of 'unsafeCoerce' to 
switch between the two without cost, it does however require us to keep the types in sync!

-}

{- Note: Divison by "almost" constants
Calculating the index into a packed vector involves dividing by (64 / bitSize) a lot, so the divisor, while not always statically known,
is at least constant for one vector object (since we cannot mutate bitSize, that requires a copy). Thus we, on creating, retrieve the
constants and later use those for division.

For vectors with fixed bit-size this lookup is done at compile time and directly inserted at all places we use divison by constants.

Ideally GHC would just do this for us, but as of now it won't. It also never inlines constant lookups into constant luts.
For integer divison by constants refer to: https://gitlab.haskell.org/ghc/ghc/-/issues/9786
-}

-- Generic vector operations
class Pack a => MPVector (v :: Type -> Type -> Type) (a :: Type) where
  -- | O(1) Length of the packed vector in elements
  length        :: v s a -> Int
  -- | O(1) Size in bits with which the elements are stored in the vector
  bitSize       :: v s a -> Int
  -- Obtain both the index of the word containing the value and the index into that word
  -- This is a class method to allow for fast division when the bit size is not a compile
  -- time constant. See note Division by "almost" constants.
  index         :: v s a -> Int -> (Int, Int)
  -- Obtain a ptr into the data and apply the returned action
  -- TODO If I also add unpinned backing storage I need to rethink this
  unsafeWithPtr :: PrimMonad m => v s a -> (Ptr Word64 -> IO x) -> m x
  -- Monadic left fold over each element. The passed function is passed the element index,
  -- the index of the word and the index into the word and the word itself for each element.
  -- The actual element can be obtained via @unpack bitSize wordIndex word@. 
  foldMapI      :: PrimBase m => (Int -> Int -> Int -> Word64 -> b -> m b) -> b -> v s a -> m b
  -- | Create a new packed vector with exactly the same shape as the given argument
  sameShape     :: PrimMonad m => v s a -> m (v (PrimState m) a)

instance
  ( Pack a
  , KnownNat len
  , KnownNat bitSize
  ) => MPVector (MutablePackedVector ('Static len) ('Static bitSize)) a where
  length _ = fromIntegral . natVal $ Proxy @len
  {-# INLINE length #-}
  bitSize _ = fromIntegral . natVal $ Proxy @bitSize
  {-# INLINE bitSize #-}
  index _ = Util.index (64 `quotInt` fromIntegral (natVal $ Proxy @bitSize))
  {-# INLINE index #-}
  unsafeWithPtr (MPVec_SS fptr) = unsafePrimToPrim . unsafeWithForeignPtr fptr
  {-# INLINE unsafeWithPtr #-}
  foldMapI f b1 v@(MPVec_SS fptr) = Util.foldMap elemsPerWord (length v) fptr b1 f
    where elemsPerWord = 64 `quotInt` fromIntegral (natVal $ Proxy @bitSize)
  {-# INLINE foldMapI #-}
  sameShape _ = new @('Static len) @('Static bitSize)
  {-# INLINE sameShape #-}

instance (Pack a, KnownNat bitSize) => MPVector (MutablePackedVector 'Dynamic ('Static bitSize)) a where
  length (MPVec_DS l _) = l
  {-# INLINE length #-}
  bitSize _ = fromIntegral . natVal $ Proxy @bitSize
  {-# INLINE bitSize #-}
  index _ = Util.index (64 `quotInt` fromIntegral (natVal $ Proxy @(bitSize)))
  {-# INLINE index #-}
  unsafeWithPtr (MPVec_DS _ fptr) = unsafePrimToPrim . unsafeWithForeignPtr fptr
  {-# INLINE unsafeWithPtr #-}
  foldMapI f b1 (MPVec_DS len fptr) = Util.foldMap elemsPerWord len fptr b1 f
    where elemsPerWord = 64 `quotInt` fromIntegral (natVal $ Proxy @bitSize)
  {-# INLINE foldMapI #-}
  sameShape v = new @'Dynamic @('Static bitSize) $ length v
  {-# INLINE sameShape #-}

instance (Pack a, KnownNat len) => MPVector (MutablePackedVector ('Static len) 'Dynamic) a where
  length _ = fromIntegral . natVal $ Proxy @len
  {-# INLINE length #-}
  bitSize (MPVec_SD bSz _ _ _ _ _) = bSz
  {-# INLINE bitSize #-}
  index (MPVec_SD _ els m a shft _) i =
    let !arrI = (Util.mulHi i m + a * i) `unsafeShiftR` shft
        !wordI = i - arrI * els
    in (arrI, wordI)
  {-# INLINE index #-}
  unsafeWithPtr (MPVec_SD _ _ _ _ _ fptr) = unsafePrimToPrim . unsafeWithForeignPtr fptr
  {-# INLINE unsafeWithPtr #-}
  foldMapI f b1 v@(MPVec_SD _ elemsPerWord _ _ _ fptr) = Util.foldMap elemsPerWord (length v) fptr b1 f
  {-# INLINE foldMapI #-}
  sameShape v = new @('Static len) @'Dynamic $ bitSize v
  {-# INLINE sameShape #-}

instance Pack a => MPVector (MutablePackedVector 'Dynamic 'Dynamic) a where
  length (MPVec_DD len _ _ _ _ _ _) = len
  {-# INLINE length #-}
  bitSize (MPVec_DD _ bSz _ _ _ _ _) = bSz
  {-# INLINE bitSize #-}
  index (MPVec_DD _ _ els m a shft _) i =
    let !arrI = (Util.mulHi i m + a * i) `unsafeShiftR` shft
        !wordI = i - arrI * els
    in (arrI, wordI)
  {-# INLINE index #-}
  unsafeWithPtr (MPVec_DD _ _ _ _ _ _ fptr) = unsafePrimToPrim . unsafeWithForeignPtr fptr
  {-# INLINE unsafeWithPtr #-}
  foldMapI f b1 (MPVec_DD len _ elemsPerWord _ _ _ fptr) = Util.foldMap elemsPerWord len fptr b1 f
  {-# INLINE foldMapI #-}
  sameShape v = new @'Dynamic @'Dynamic (length v) (bitSize v)
  {-# INLINE sameShape #-}

-- creation

-- | Type family to unify the different types of vectors. Vectors that lack compile time information need
-- to be given more arguments for creation. See 'CreatePacked' and 'new' for how this is implemented.
type family CreateFn (sz :: DynamicNat) (bSz :: DynamicNat) (m :: Type -> Type) (a :: Type) :: Type where
  CreateFn ('Static sz) ('Static bSz) m a =               m (MutablePackedVector ('Static sz) ('Static bSz) (PrimState m) a)
  CreateFn ('Static sz)  'Dynamic     m a = Int ->        m (MutablePackedVector ('Static sz)  'Dynamic     (PrimState m) a)
  CreateFn 'Dynamic     ('Static bSz) m a = Int ->        m (MutablePackedVector  'Dynamic    ('Static bSz) (PrimState m) a)
  CreateFn 'Dynamic      'Dynamic     m a = Int -> Int -> m (MutablePackedVector  'Dynamic     'Dynamic     (PrimState m) a)

-- | Type family to unify the creation of packed vectors from existing foreign pointers.
type family FromForeignPtrFn (sz :: DynamicNat) (bSz :: DynamicNat) s (a :: Type) :: Type where
  FromForeignPtrFn ('Static sz) ('Static bSz) s a =               ForeignPtr Word64 -> MutablePackedVector ('Static sz) ('Static bSz) s a
  FromForeignPtrFn ('Static sz)  'Dynamic     s a = Int ->        ForeignPtr Word64 -> MutablePackedVector ('Static sz)  'Dynamic     s a
  FromForeignPtrFn 'Dynamic     ('Static bSz) s a = Int ->        ForeignPtr Word64 -> MutablePackedVector  'Dynamic    ('Static bSz) s a
  FromForeignPtrFn 'Dynamic      'Dynamic     s a = Int -> Int -> ForeignPtr Word64 -> MutablePackedVector  'Dynamic     'Dynamic     s a

-- | Closed typeclass which handles creating new packed vectors
-- 'new' is effectively an overloaded method to allow passing runtime information if necessary.
-- Intended to be used with 'TypeApplications'
class CreatePacked (sz :: DynamicNat) (bSz :: DynamicNat) where
  -- | Create a new mutable packed vector. This method requires two type annotations (or a fixed inferred type)
  -- and 0-2 arguments depending on how many statically known arguments were given.
  --
  -- Usage:
  --
  -- @
  -- new @'Dynamic \@'Dynamic 10 4 
  -- @
  -- Creates a vector with 10 elements that are stored with 4 bits each.
  --
  -- @ 
  -- new @'Dynamic @('Static 4) 10
  -- @
  -- Again creates a vector with 10 elements at 4 bits each, however the bit size is statically
  -- known and the compiler can optimze the indexing and several other methods because of that.
  new :: PrimMonad m => CreateFn sz bSz m a
  -- | Create a new mutable packed vector from an existing foreign pointer. As with 'new' it requires
  -- two type annotations and 0-2 arguments.
  --
  -- The foreign pointer must be of correct size. This is not checked.
  unsafeFromForeignPtr :: FromForeignPtrFn sz bSz s a


instance (KnownNat sz, KnownNat bSz) => CreatePacked ('Static sz) ('Static bSz) where
  new = unsafePrimToPrim $ do
    fptr <- mallocPlainForeignPtrBytes (nrOfWords * 8)
    unsafeWithForeignPtr fptr $ \ptr -> Ptr.fillBytes ptr 0 (nrOfWords * 8)
    pure $ MPVec_SS fptr
    where
      sz = fromIntegral $ natVal (Proxy @sz)
      bSz = fromIntegral $ natVal (Proxy @bSz)
      perWord = 64 `quotInt` bSz
      nrOfWords = (sz + perWord - 1) `quotInt` perWord
  {-# INLINE new #-}
  unsafeFromForeignPtr = coerce
  {-# INLINE unsafeFromForeignPtr #-}

instance KnownNat bSz => CreatePacked 'Dynamic ('Static bSz) where
  new sz = unsafePrimToPrim $ do
    fptr <- mallocPlainForeignPtrBytes (nrOfWords * 8)
    unsafeWithForeignPtr fptr $ \ptr -> Ptr.fillBytes ptr 0 (nrOfWords * 8)
    pure $ MPVec_DS sz fptr
    where
      bSz = fromIntegral $ natVal (Proxy @bSz)
      perWord = 64 `quotInt` bSz
      nrOfWords = (sz + perWord - 1) `quotInt` perWord
  {-# INLINE new #-}
  unsafeFromForeignPtr = MPVec_DS
  {-# INLINE unsafeFromForeignPtr #-}

instance KnownNat sz => CreatePacked ('Static sz) 'Dynamic where
  new bSz = unsafePrimToPrim $ do
    fptr <- mallocPlainForeignPtrBytes (nrOfWords * 8)
    unsafeWithForeignPtr fptr $ \ptr -> Ptr.fillBytes ptr 0 (nrOfWords * 8)
    pure $ MPVec_SD bSz perWord m r z fptr
    where
      sz = fromIntegral $ natVal (Proxy @sz)
      perWord = 64 `quotInt` bSz
      (m,r,z) = Util.divConstants perWord
      nrOfWords = (sz + perWord - 1) `quotInt` perWord
  {-# INLINE new #-}
  unsafeFromForeignPtr bsz = MPVec_SD bsz perWord m r z
    where
      perWord = 64 `quotInt` bsz
      (m,r,z) = Util.divConstants perWord
  {-# INLINE unsafeFromForeignPtr #-}

instance CreatePacked 'Dynamic 'Dynamic where
  new sz bSz = unsafePrimToPrim $ do
    fptr <- mallocPlainForeignPtrBytes (nrOfWords * 8)
    unsafeWithForeignPtr fptr $ \ptr -> Ptr.fillBytes ptr 0 (nrOfWords * 8)
    pure $ MPVec_DD sz bSz perWord m r z fptr
    where
      perWord = 64 `quotInt` bSz
      (m,r,z) = Util.divConstants perWord
      nrOfWords = (sz + perWord - 1) `quotInt` perWord
  {-# INLINE new #-}
  unsafeFromForeignPtr sz bsz = MPVec_DD sz bsz perWord m r z
    where
      perWord = 64 `quotInt` bsz
      (m,r,z) = Util.divConstants perWord
  {-# INLINE unsafeFromForeignPtr #-}

-- Operations

-- | @O(1)@ Test if the vector is empty, its length is 0.
null :: MPVector v a => v s a -> Bool
null v = length v == 0
{-# INLINE null #-}

-- * Access to individual elements

-- | @O(1)@ Read an element at the index
-- 
-- Does not check bounds.
unsafeRead :: (PrimMonad m, MPVector v a) => v (PrimState m) a -> Int -> m a
unsafeRead v i =
  let (arrI, wordI) = index v i
      bitSz = bitSize v
  in unsafeWithPtr v $ \ptr -> unpack bitSz wordI <$> S.peekElemOff ptr arrI
{-# INLINE unsafeRead #-}

-- | @O(1)@ Read an element at the index
-- 
-- Does a bounds check before reading and errors if outside of bounds.
read :: (PrimMonad m, MPVector v a) => v (PrimState m) a -> Int -> m a
read v i = checkBounds "read" (length v) i $ unsafeRead v i
{-# INLINE read #-}

-- | @O(1)@ Write an element to the index
--
-- Does not check bounds.
unsafeWrite :: (PrimMonad m, MPVector v a) => v (PrimState m) a -> Int -> a -> m ()
unsafeWrite v i el =
  let (arrI, wordI) = index v i
      bitSz = bitSize v
  in unsafeWithPtr v $ \ptr -> do
    w <- S.peekElemOff ptr arrI
    S.pokeElemOff ptr arrI $ packInto bitSz wordI w el
{-# INLINE unsafeWrite #-}

-- | @O(1)@ Write an element to the index
--
-- Does a bounds check before writing and errors if outside of bounds.
write :: (PrimMonad m, MPVector v a) => v (PrimState m) a -> Int -> a -> m ()
write v i el = checkBounds "write" (length v) i $ unsafeWrite v i el
{-# INLINE write #-}

-- | @O(1)@ Modify an element at the index
--
-- Does not check bounds.
unsafeModify :: (PrimMonad m, MPVector v a) => v (PrimState m) a -> Int -> (a -> a) -> m ()
-- TODO Fuse to reuse to reuse the read word? I don't know if ghc can do this for us already...
unsafeModify v i f = unsafeRead v i >>= unsafeWrite v i . f
{-# INLINE unsafeModify #-}

-- | @O(1)@ Modify an element at the index
--
-- Does a bounds check before reading and writing and errors if outside of bounds.
modify :: (PrimMonad m, MPVector v a) => v (PrimState m) a -> Int -> (a -> a) -> m ()
modify v i f = checkBounds "modify" (length v) i $ unsafeModify v i f
{-# INLINE modify #-}

-- | @O(1)@ Monadically modify an element at the index
--
-- Does not check bounds.
unsafeModifyM :: (PrimMonad m, MPVector v a) => v (PrimState m) a -> Int -> (a -> m a) -> m ()
unsafeModifyM v i f = unsafeRead v i >>= f >>= unsafeWrite v i
{-# INLINE unsafeModifyM #-}

-- | @O(1)@ Modify an element at the index
--
-- Does a bounds check before reading and writing and errors if outside of bounds.
modifyM :: (PrimMonad m, MPVector v a) => v (PrimState m) a -> Int -> (a -> m a) -> m ()
modifyM v i f = checkBounds "modifyM" (length v) i $ unsafeModifyM v i f
{-# INLINE modifyM #-}

-- | @O(1)@ Swap the values at the two indizes
--
-- Does not check bounds.
unsafeSwap :: (PrimMonad m, MPVector v a) => v (PrimState m) a -> Int -> Int -> m ()
unsafeSwap v i j = do
  a <- unsafeRead v i
  b <- unsafeRead v j
  unsafeWrite v i b
  unsafeWrite v j a
{-# INLINE unsafeSwap #-}

-- | @O(1)@ Swap the values at the two indizes
--
-- Does a bounds check for each index before swapping the values and errors if either one is outside of bounds.
swap :: (PrimMonad m, MPVector v a) => v (PrimState m) a -> Int -> Int -> m ()
swap v i j = checkBounds "swap" (length v) i . checkBounds "swap" (length v) j $ unsafeSwap v i j
{-# INLINE swap #-}

-- | @O(1)@ Write an element to the index and return the previous element
--
-- Does not check bounds.
unsafeExchange :: (PrimMonad m, MPVector v a) => v (PrimState m) a -> Int -> a -> m a
unsafeExchange v i el = do
  old <- unsafeRead v i
  unsafeWrite v i el
  pure old
{-# INLINE unsafeExchange #-}


-- | @O(1)@ Write an element to the index and return the previous element
--
-- Does a bounds check and errors if outside of bounds.
exchange :: (PrimMonad m, MPVector v a) => v (PrimState m) a -> Int -> a -> m a
exchange v i el = checkBounds "exhange" (length v) i $ unsafeExchange v i el
{-# INLINE exchange #-}

-- * Folds

-- | @O(n)@ Indexed monadic left fold over all elements
ifoldM :: (PrimBase m, MPVector v a) => (Int -> b -> a -> m b) -> b -> v (PrimState m) a -> m b
ifoldM f b1 v = foldMapI (\ind _ wordI w b -> f ind b (unpack bSz wordI w)) b1 v
  where bSz = bitSize v
{-# INLINE ifoldM #-}

-- | @O(n)@ Monadic left fold over all elements
foldM :: (PrimBase m, MPVector v a) => (b -> a -> m b) -> b -> v (PrimState m) a -> m b
foldM f = ifoldM (\_ b a -> f b a)
{-# INLINE foldM #-}

-- | @O(n)@ Indexed traversal over all elements
itraverse_ :: (PrimBase m, MPVector v a) => (Int -> a -> m b) -> v (PrimState m) a -> m ()
itraverse_ f = ifoldM (\i _ a -> void $ f i a) ()
{-# INLINE itraverse_ #-}

-- | @O(n)@ Traversal over all elements
traverse_ :: (PrimBase m, MPVector v a) => (a -> m b) -> v (PrimState m) a -> m ()
traverse_ = itraverse_ . const
{-# INLINE traverse_ #-}

-- | @O(n)@ Indexed strict left fold
ifoldl' :: forall v a b s . MPVector v a => (Int -> b -> a -> b) -> b -> v s a -> b
ifoldl' f b1 v = unsafePerformIO $ ifoldM (\i b a -> pure @IO $ f i b a) b1 (unsafeCoerce @_ @(v RealWorld a) v)
{-# INLINE ifoldl' #-}

-- | @O(n)@ Strict left fold
foldl' :: MPVector v a => (b -> a -> b) -> b -> v s a -> b
foldl' = ifoldl' . const
{-# INLINE foldl' #-}

-- * Copy

-- | Copy all elements from the second argument (the source) into the first argument (the destination).
--
-- This method requires both vectors to have the same size, but that is not checked.
--
-- While both vectors must have the same size, everything else may vary, including the bit size.
--
-- Copying from a higher bit size down to a lower bit size will truncate the elements to the new bit size.
unsafeCopy :: forall v w a m . (PrimMonad m, MPVector v a, MPVector w a) => v (PrimState m) a -> w (PrimState m) a -> m ()
unsafeCopy dst src
  | dstBSz == bitSize src = unsafeWithPtr dst $ \dstP -> unsafeWithPtr src $ \srcP -> copyBytes dstP srcP $ nrOfWords src * 8
  | otherwise   = unsafePrimToPrim $ itraverse_ (unsafeWrite @IO ioDst) ioSrc
  where
    ioDst = unsafeCoerce @_ @(v RealWorld a) dst
    ioSrc = unsafeCoerce @_ @(w RealWorld a) src
    dstBSz = bitSize dst
    nrOfWords v = let perWord = 64 `div` bitSize v in (length v + perWord - 1) `quotInt` perWord
    {-# INLINE nrOfWords #-}
{-# INLINE unsafeCopy #-}

-- | Copy all elements from the second argument (the source) into the first argument (the destination).
--
-- This method requires both vectors to have the same size and errors if that is not the case.
--
-- While both vectors must have the same size, everything else may vary, including the bit size.
--
-- Copying from a higher bit size down to a lower bit size will truncate the elements to the new bit size.
copy :: (PrimMonad m, MPVector v a, MPVector w a) => v (PrimState m) a -> w (PrimState m) a -> m ()
copy v w
  | len1 == len2 = unsafeCopy v w
  | otherwise    = error "Data.PackedVector.Mutable:copy vectors have different lengths"
  where
    len1 = length v
    len2 = length w
{-# INLINE copy #-}

-- Utilities
checkBounds :: String -> Int -> Int -> a -> a
checkBounds func len i r
  | 0 <= i && i < len = r
  | otherwise         = indexOutOfBounds func len i
{-# INLINE checkBounds #-}

indexOutOfBounds :: String -> Int -> Int -> a
indexOutOfBounds func len i = error $ "Data.PackedVector.Mutable:" <> func <> " index " <> show i <> " of bounds [0," <> show len <> ")" 
{-# INLINE indexOutOfBounds #-}
