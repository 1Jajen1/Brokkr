{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Brokkr.PackedVector.Internal (
  DynamicNat(..)
, PackedVector(..)
, Mutable
, PVector(..)
-- creation
, fromList
, unsafeFromForeignPtr
-- length
, null
-- indexing
, unsafeIndex, (!), (!?)
-- copy
, thaw
, freeze
-- folds
, toList
) where

import Data.Kind

import Brokkr.PackedVector.Mutable (DynamicNat(..))
import Brokkr.PackedVector.Mutable.Internal (MutablePackedVector(..), MPVector)
import Control.Monad.Primitive
import Foreign.ForeignPtr (ForeignPtr)
import Data.Word
import qualified Brokkr.PackedVector.Mutable as M
import GHC.TypeLits
import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)
import Data.Coerce (coerce)
import Prelude hiding (null, length)
import Control.Monad.ST (runST)
import Brokkr.PackedVector.Pack
import Control.Monad (forM_)
import qualified Brokkr.PackedVector.Util as Util
import Control.DeepSeq

-- | Packed vector. Stores elements which require less than 64 bits efficiently.
--
-- Elements are stored in 64 bit words, this means @ 64 % bitSize @ bits will be unused in each 64 bit word.
-- Each combination of 'vecSize' and 'bitSize' will yield a type with minimal runtime information required for
-- efficient use.
data family PackedVector (vecSize :: DynamicNat) (bitSize :: DynamicNat) (a :: Type)

newtype instance PackedVector ('Static len) ('Static bitSize) (a :: Type) = PVec_SS (ForeignPtr Word64) 
  deriving newtype Eq

data instance PackedVector 'Dynamic ('Static bSz) (a :: Type) =
    PVec_DS
      {-# UNPACK #-} !Int -- size in elements
      {-# UNPACK #-} !(ForeignPtr Word64)
  deriving stock Eq

data instance PackedVector ('Static len) 'Dynamic (a :: Type) =
    PVec_SD
      {-# UNPACK #-} !Int -- Element bit size
      -- See note [Divison by "almost" constants]
      {-# UNPACK #-} !Int -- Elements per word
      {-# UNPACK #-} !Int -- Mult. constant for 1/ (64 / bitSize)
      {-# UNPACK #-} !Int -- Add. constant ^
      {-# UNPACK #-} !Int -- Bitshift constant
      {-# UNPACK #-} !(ForeignPtr Word64)
  deriving stock Eq

data instance PackedVector 'Dynamic 'Dynamic (a :: Type) =
    PVec_DD 
      {-# UNPACK #-} !Int -- Size in elements
      {-# UNPACK #-} !Int -- Element bit size
      -- See note [Divison by "almost" constants]
      {-# UNPACK #-} !Int -- Elements per word
      {-# UNPACK #-} !Int -- Mult. constant for 1/ (64 / bitSize)
      {-# UNPACK #-} !Int -- Add. constant ^
      {-# UNPACK #-} !Int -- Bitshift constant
      {-# UNPACK #-} !(ForeignPtr Word64)
  deriving stock Eq

instance NFData (PackedVector ('Static sz) ('Static bSz) a) where
  rnf (PVec_SS _) = ()
instance NFData (PackedVector ('Static sz) 'Dynamic a) where
  rnf PVec_SD {} = ()
instance NFData (PackedVector 'Dynamic ('Static bSz) a) where
  rnf (PVec_DS _ _) = ()
instance NFData (PackedVector 'Dynamic 'Dynamic a) where
  rnf PVec_DD {} = ()

type family Mutable (v :: Type -> Type) = (mv :: Type -> Type -> Type) | mv -> v

type instance Mutable (PackedVector vecSize bitSize) = M.MutablePackedVector vecSize bitSize

class (MPVector (Mutable v) a) => PVector (v :: Type -> Type) (a :: Type) where
  -- | @O(1)@ Returns the length of the packed vector in elements
  length        :: v a -> Int
  -- | @O(1)@ Returns the bit size of the elements in the packed vector
  bitSize       :: v a -> Int
  -- | @O(1)@ Create a mutable packed vector from an immutable packed vector without copying.
  --
  -- This method is effectively free since it is usually implemented via 'unsafeCoerce'.
  unsafeThaw    :: PrimMonad m => v a -> m ((Mutable v) (PrimState m) a)
  -- | @O(1)@ Create an immutable packed vector from a mutable packed vector without copying.
  --
  -- This method is effectively free since it is usually implemented via 'unsafeCoerce'.
  unsafeFreeze  :: PrimMonad m => (Mutable v) (PrimState m) a -> m (v a)

instance
  ( Pack a
  , KnownNat len
  , KnownNat bitSize
  , KnownNat (Div 64 bitSize)
  ) => PVector (PackedVector ('Static len) ('Static bitSize)) (a :: Type) where
    length _ = fromIntegral . natVal $ Proxy @len
    {-# INLINE length #-}
    bitSize _ = fromIntegral . natVal $ Proxy @bitSize
    {-# INLINE bitSize #-}
    unsafeThaw = pure . coerce
    {-# INLINE unsafeThaw #-}
    unsafeFreeze = pure . coerce
    {-# INLINE unsafeFreeze #-}

instance (Pack a, KnownNat bitSize, KnownNat (Div 64 bitSize)) => PVector (PackedVector 'Dynamic ('Static bitSize)) a where
  length (PVec_DS len _) = len
  {-# INLINE length #-}
  bitSize _ = fromIntegral . natVal $ Proxy @bitSize
  {-# INLINE bitSize #-}
  unsafeThaw !v = pure $ unsafeCoerce v
  {-# INLINE unsafeThaw #-}
  unsafeFreeze !w = pure $ unsafeCoerce w
  {-# INLINE unsafeFreeze #-}

instance (Pack a, KnownNat len) => PVector (PackedVector ('Static len) 'Dynamic) a where
  length _ = fromIntegral . natVal $ Proxy @len
  {-# INLINE length #-}
  bitSize (PVec_SD bSz _ _ _ _ _) = bSz
  {-# INLINE bitSize #-}
  unsafeThaw !v = pure $ unsafeCoerce v
  {-# INLINE unsafeThaw #-}
  unsafeFreeze !w = pure $ unsafeCoerce w
  {-# INLINE unsafeFreeze #-}

instance Pack a => PVector (PackedVector 'Dynamic 'Dynamic) a where
  length (PVec_DD len _ _ _ _ _ _) = len
  {-# INLINE length #-}
  bitSize (PVec_DD _ bSz _ _ _ _ _) = bSz
  {-# INLINE bitSize #-}
  unsafeThaw !v = pure $ unsafeCoerce v
  {-# INLINE unsafeThaw #-}
  unsafeFreeze !w = pure $ unsafeCoerce w
  {-# INLINE unsafeFreeze #-}

-- create
type family CreateFn (sz :: DynamicNat) (bSz :: DynamicNat) (a :: Type) :: Type where
  CreateFn ('Static sz) ('Static bSz) a =               PackedVector ('Static sz) ('Static bSz) a
  CreateFn ('Static sz)  'Dynamic     a = Int ->        PackedVector ('Static sz)  'Dynamic     a
  CreateFn 'Dynamic     ('Static bSz) a = Int ->        PackedVector  'Dynamic    ('Static bSz) a
  CreateFn 'Dynamic      'Dynamic     a = Int -> Int -> PackedVector  'Dynamic     'Dynamic     a

class CreatePacked (sz :: DynamicNat) (bSz :: DynamicNat) where
  fromList :: Pack a => [a] -> CreateFn sz bSz a
  unsafeFromForeignPtr :: ForeignPtr Word64 -> CreateFn sz bSz a

instance (KnownNat sz, KnownNat bSz, KnownNat (Div 64 bSz)) => CreatePacked ('Static sz) ('Static bSz) where
  fromList xs = runST $ do
    mpv <- M.new @('Static sz) @('Static bSz)
    forM_ (zip [0..] xs) $ \(i, el) -> do
      M.unsafeWrite mpv i el
    unsafeFreeze mpv
  {-# INLINE fromList #-}
  unsafeFromForeignPtr = coerce
  {-# INLINE unsafeFromForeignPtr #-}

instance (KnownNat bSz, KnownNat (Div 64 bSz)) => CreatePacked 'Dynamic ('Static bSz) where
  fromList xs sz = runST $ do
    mpv <- M.new @'Dynamic @('Static bSz) sz
    forM_ (zip [0..] xs) $ \(i, el) -> do
      M.unsafeWrite mpv i el
    unsafeFreeze mpv
  {-# INLINE fromList #-}
  unsafeFromForeignPtr fptr sz = PVec_DS sz fptr
  {-# INLINE unsafeFromForeignPtr #-}

instance KnownNat sz => CreatePacked ('Static sz) 'Dynamic where
  fromList xs bSz = runST $ do
    mpv <- M.new @('Static sz) @'Dynamic bSz
    forM_ (zip [0..] xs) $ \(i, el) -> do
      M.unsafeWrite mpv i el
    unsafeFreeze mpv
  {-# INLINE fromList #-}
  unsafeFromForeignPtr fptr bSz = PVec_SD bSz perWord m r z fptr
    where
      perWord = 64 `div` bSz
      (m,r,z) = Util.divConstants perWord
  {-# INLINE unsafeFromForeignPtr #-}

instance CreatePacked 'Dynamic 'Dynamic where
  fromList xs sz bSz = runST $ do
    mpv <- M.new @'Dynamic @'Dynamic sz bSz
    forM_ (zip [0..] xs) $ \(i, el) -> do
      M.unsafeWrite mpv i el
    unsafeFreeze mpv
  {-# INLINE fromList #-}
  unsafeFromForeignPtr fptr sz bSz = PVec_DD sz bSz perWord m r z fptr
    where
      perWord = 64 `div` bSz
      (m,r,z) = Util.divConstants perWord
  {-# INLINE unsafeFromForeignPtr #-}

-- derived methods

-- | @O(1)@ Check if the vector is empty
null :: PVector v a => v a -> Bool
null v = length v == 0
{-# INLINE null #-}

-- * Indexing

-- | @O(1)@ Read the element at the index
--
-- Does not check bounds.
unsafeIndex :: PVector v a => v a -> Int -> a
unsafeIndex v i = runST $ unsafeThaw v >>= flip M.unsafeRead i
{-# INLINE unsafeIndex #-}

infixl 9 !
-- | @O(1)@ Read the element at the index
--
-- Does a bounds check and errors if outside of bounds.
(!) :: PVector v a => v a -> Int -> a
(!) v i = checkBounds "(!)" (length v) i $ unsafeIndex v i
{-# INLINE (!) #-}

infixl 9 !?
-- | @O(1)@ Read the element at the index
--
-- Does a bounds check and returns 'Nothing' if outside of bounds.
(!?) :: PVector v a => v a -> Int -> Maybe a
(!?) v i | 0 <= i && i < length v = Just $ unsafeIndex v i
         | otherwise              = Nothing
{-# INLINE (!?) #-}

-- * Copy

-- | Create a mutable vector from an immutable vector with copying.
thaw :: (PrimMonad m, PVector v a) => v a -> m ((Mutable v) (PrimState m) a)
thaw v = do
  mV <- unsafeThaw v
  newV <- M.sameShape mV
  M.unsafeCopy newV mV
  pure newV

-- | Create an immutable vector from a mutable vector with copying.
freeze :: (PrimMonad m, PVector v a) => (Mutable v) (PrimState m) a -> m (v a)
freeze mV = do
  newV <- M.sameShape mV
  M.unsafeCopy newV mV
  unsafeFreeze newV

-- TODO More folds and avoid using the mutable variant, so built them on foldMap/foldl' more

toList :: PVector v a => v a -> [a]
toList v = runST $ do
  mpv <- unsafeThaw v
  pure $ M.foldl' (\acc el -> acc <> [el]) [] mpv

-- other instances
instance (KnownNat sz, KnownNat bSz, KnownNat (Div 64 bSz), Show a, Pack a) => Show (PackedVector ('Static sz) ('Static bSz) a) where
  show v = "PackedVector " <> show (length v) <> ".S " <> show (bitSize v) <> ".S " <> show (toList v)

instance (KnownNat bSz, KnownNat (Div 64 bSz), Show a, Pack a) => Show (PackedVector 'Dynamic ('Static bSz) a) where
  show v = "PackedVector " <> show (length v) <> ".D " <> show (bitSize v) <> ".S " <> show (toList v)

instance (KnownNat sz, Show a, Pack a) => Show (PackedVector ('Static sz) 'Dynamic a) where
  show v = "PackedVector " <> show (length v) <> ".S " <> show (bitSize v) <> ".D " <> show (toList v)

instance (Show a, Pack a) => Show (PackedVector 'Dynamic 'Dynamic a) where
  show v = "PackedVector " <> show (length v) <> ".D " <> show (bitSize v) <> ".D " <> show (toList v)

--
checkBounds :: String -> Int -> Int -> a -> a
checkBounds func len i r
  | 0 <= i && i < len = r
  | otherwise         = indexOutOfBounds func len i
{-# INLINE checkBounds #-}

indexOutOfBounds :: String -> Int -> Int -> a
indexOutOfBounds func len i = error $ "Data.PackedVector:" <> func <> " index " <> show i <> " of bounds [0," <> show len <> ")" 
