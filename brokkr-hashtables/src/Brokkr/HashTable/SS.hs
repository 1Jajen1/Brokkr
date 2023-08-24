{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Brokkr.HashTable.SS (
  HashTable'(..)
, new
, size
, lookup, lookupWithHash
, insert, insertWithHash
, delete, deleteWithHash
, reserve
, foldM
) where

import Prelude hiding (lookup)

import Brokkr.HashTable.Common qualified as Common
import Brokkr.HashTable.Internal (Storage(..), HashTable', Hash(..), HashFn(..), Salt, MaxLoadFactor)

import Control.Monad.Primitive

import Data.Bits
import Data.Int

import Data.Primitive
import Data.Primitive.PrimVar

import GHC.Exts
import Control.Monad (when)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Storable qualified as Storable

type HashTable s key value = HashTable' Storable Storable s key value

data instance HashTable' Storable Storable s key value =
  HashTable_SS {
    hashSalt       :: {-# UNPACK #-} !Salt
  , maxLoadFactor  :: {-# UNPACK #-} !MaxLoadFactor
  , sizeRef        :: {-# UNPACK #-} !(PrimVar s Int32) -- how many elements
  , capacityRef    :: {-# UNPACK #-} !(PrimVar s Int32) -- whats our capacity. The actual array size is this plus the max distance
  , maxDistanceRef :: {-# UNPACK #-} !(PrimVar s Int8) -- how many elements we can be away from the ideal spot before resizing
    -- Uses MutVar# to store an unlifted MutableArray# which otherwise would need to be boxed and allocated
  , backingRef :: MutVar# s (MutableByteArray# s)
  }

new :: forall key value m . (PrimMonad m, Storable.Storable key, Storable.Storable value) => Salt -> MaxLoadFactor -> m (HashTable (PrimState m) key value)
{-# INLINE new #-}
new hashSalt maxLoadFactor = do
  sizeRef <- newPrimVar 0
  maxDistanceRef <- newPrimVar $ fromIntegral initMaxDistance
  capacityRef <- newPrimVar $ fromIntegral initCap
  MutableByteArray distArr <- newAlignedPinnedByteArray backingSz (alignmentElement @key @value)
  t <- primitive $ \s -> case newMutVar# distArr s of
    (# s1, backingRef #) -> (# s1, HashTable_SS{..} #)
  _ <- unsafeIOToPrim $ memset (Ptr (mutableByteArrayContents# distArr)) (-1) (fromIntegral backingSz)
  pure t
  where
    initCap = 32
    initMaxDistance = 5
    initArrSz = initCap + initMaxDistance
    backingSz = initArrSz * sizeElement @key @value

alignmentElement :: forall key val . Storable.Storable key => Storable.Storable val => Int
alignmentElement = max alignKey alignVal
  where
    alignKey = Storable.alignment (undefined :: key)
    alignVal = Storable.alignment (undefined :: val)

sizeElement :: forall key val . Storable.Storable key => Storable.Storable val => Int
sizeElement = sizeElement' + getEndPad sizeElement' alignFinal
  where
    alignKey = Storable.alignment (undefined :: key)
    alignVal = Storable.alignment (undefined :: val)
    sizeKey = Storable.sizeOf (undefined :: key)
    sizeVal = Storable.sizeOf (undefined :: val)
    getEndPad sz al =
      let extra = sz `rem` al
      in if extra == 0 then 0 else al
    sizeDistAndKey = sizeKey + alignKey
    sizeElement' = sizeDistAndKey + getEndPad sizeDistAndKey alignVal + sizeVal
    alignFinal = max alignKey alignVal

valueOffset :: forall key val . Storable.Storable key => Storable.Storable val => Int
valueOffset = sizeDistAndKey + getEndPad sizeDistAndKey alignVal
  where
    alignKey = Storable.alignment (undefined :: key)
    alignVal = Storable.alignment (undefined :: val)
    sizeKey = Storable.sizeOf (undefined :: key)
    getEndPad sz al =
      let extra = sz `rem` al
      in if extra == 0 then 0 else al
    sizeDistAndKey = sizeKey + alignKey

keyOffset :: forall key . Storable.Storable key => Int
keyOffset = Storable.alignment (undefined :: key)

size :: PrimMonad m => HashTable (PrimState m) key value -> m Int
size HashTable_SS{..} = fromIntegral <$> readPrimVar sizeRef

lookup
  :: ( PrimMonad m, Eq key, Hash key, Storable.Storable key, Storable.Storable value )
  => HashTable (PrimState m) key value -> key -> m (Maybe value)
{-# INLINE lookup #-}
lookup ht key = lookupWithHash ht key (coerce (hash key) (hashSalt ht))

lookupWithHash
  :: forall key value m
  . ( PrimMonad m, Eq key, Storable.Storable key, Storable.Storable value )
  => HashTable (PrimState m) key value -> key -> Int -> m (Maybe value)
{-# INLINE lookupWithHash #-}
lookupWithHash HashTable_SS{..} key !hs = do
  MutablePrimArray backingArr# <- primitive $ \s -> case readMutVar# backingRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  let backingArr = Ptr (mutableByteArrayContents# backingArr#)
  capacity <- fromIntegral <$> readPrimVar capacityRef
  Common.lookupWithHash
    (\n -> unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value)
    (\n -> unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value + keyOffset @key)
    (\n -> unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value + valueOffset @key @value)
    capacity key hs

insert :: (PrimMonad m, Eq key, Hash key, Storable.Storable key, Storable.Storable value) => HashTable (PrimState m) key value -> key -> value -> m ()
{-# INLINE insert #-}
insert ht key = insertWithHash ht key (coerce (hash key) (hashSalt ht))

insertWithHash
  :: forall key value m
  . ( PrimMonad m, Eq key, Hash key, Storable.Storable key, Storable.Storable value )
  => HashTable (PrimState m) key value -> key -> Int -> value -> m ()
{-# INLINEABLE insertWithHash #-}
insertWithHash ht@HashTable_SS{..} k0 !hs v0 = do
  MutablePrimArray backingArr# <- primitive $ \s -> case readMutVar# backingRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  let backingArr = Ptr (mutableByteArrayContents# backingArr#)
  maxDistance <- readPrimVar maxDistanceRef
  capacity <- readPrimVar capacityRef
  Common.insertWithHash
    (\k v -> grow ht >> insert ht k v)
    (readPrimVar sizeRef >>= \sz -> writePrimVar sizeRef (sz + 1) >> pure (fromIntegral sz))
    (\n -> unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value)
    (\n -> unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value + keyOffset @key)
    (\n -> unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value + valueOffset @key @value)
    (\n v -> unsafeIOToPrim $ Storable.pokeByteOff backingArr (n * sizeElement @key @value) v)
    (\n v -> unsafeIOToPrim $ Storable.pokeByteOff backingArr (n * sizeElement @key @value + keyOffset @key) v)
    (\n v -> unsafeIOToPrim $ Storable.pokeByteOff backingArr (n * sizeElement @key @value + valueOffset @key @value) v)
    maxLoadFactor maxDistance (fromIntegral capacity) k0 hs v0

grow
  :: ( PrimMonad m, Eq key, Hash key, Storable.Storable key, Storable.Storable value )
  => HashTable (PrimState m) key value -> m ()
{-# INLINE grow #-}
grow ht@HashTable_SS{..} = do
  capacity <- fromIntegral <$> readPrimVar capacityRef
  growTo ht $ capacity * 2

-- Invariant: the new size is always a power of 2, otherwise countTrailingZeros is wrong
growTo
  :: forall key value m
  . ( PrimMonad m, Eq key, Hash key, Storable.Storable key, Storable.Storable value )
  => HashTable (PrimState m) key value -> Int -> m ()
{-# INLINEABLE growTo #-}
growTo ht@HashTable_SS{..} newSz = do
  MutablePrimArray backingArr# <- primitive $ \s -> case readMutVar# backingRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  let backingArr = Ptr (mutableByteArrayContents# backingArr#)

  oldMaxDistance <- fromIntegral <$> readPrimVar maxDistanceRef
  capacity <- fromIntegral <$> readPrimVar capacityRef

  when (capacity < newSz) $ do
    let newArrSize = newSz + fromIntegral newMaxDistance
        newMaxDistance = fromIntegral $ countTrailingZeros newSz

    (MutableByteArray newBackingArr#) <- newAlignedPinnedByteArray (newArrSize * sizeElement @key @value) (alignmentElement @key @value)
    _ <- unsafeIOToPrim $ memset (Ptr (mutableByteArrayContents# newBackingArr#)) (-1) (fromIntegral $ newArrSize * sizeElement @key @value)
    
    writePrimVar sizeRef 0
    writePrimVar maxDistanceRef newMaxDistance
    writePrimVar capacityRef (fromIntegral newSz)
    primitive $ \s -> (# writeMutVar# backingRef newBackingArr# s, () #)

    let go n
          | n >= capacity + oldMaxDistance = pure ()
          | otherwise = do
            distance :: Int8 <- unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value
            if distance == -1
              then go (n + 1)
              else do
                key <- unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value + keyOffset @key
                val <- unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value + valueOffset @key @value
                insert ht key val
                go (n + 1)
    go 0

reserve
  :: ( PrimMonad m, Eq key, Hash key, Storable.Storable key, Storable.Storable value )
  => HashTable (PrimState m) key value -> Int -> m ()
{-# INLINE reserve #-}
reserve ht@HashTable_SS{..} sz0 = do
  capacity <- fromIntegral <$> readPrimVar capacityRef
  when (capacity < newSz) $ growTo ht newSz
  where
    sz :: Int = floor $ fromIntegral sz0 / maxLoadFactor
    newSz = if sz == 1 then 1 else 1 `unsafeShiftL` (8 * sizeOf (undefined :: Int) - countLeadingZeros (sz - 1))

delete
  :: ( PrimMonad m, Eq key, Hash key, Storable.Storable key, Storable.Storable value )
  => HashTable (PrimState m) key value -> key -> m (Maybe value)
{-# INLINE delete #-}
delete ht k = deleteWithHash ht k (coerce (hash k) (hashSalt ht)) 

deleteWithHash
  :: forall key value m
  . ( PrimMonad m, Eq key, Storable.Storable key, Storable.Storable value )
  => HashTable (PrimState m) key value -> key -> Int -> m (Maybe value)
{-# INLINE deleteWithHash #-}
deleteWithHash HashTable_SS{..} k !hs = do
  MutablePrimArray backingArr# <- primitive $ \s -> case readMutVar# backingRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  let backingArr = Ptr (mutableByteArrayContents# backingArr#)

  capacity <- readPrimVar capacityRef
  maxDistance <- readPrimVar maxDistanceRef
  Common.deleteWithHash
    (readPrimVar sizeRef >>= \sz -> writePrimVar sizeRef (sz - 1))
    (\n -> unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value)
    (\n -> unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value + keyOffset @key)
    (\n -> unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value + valueOffset @key @value)
    (\n v -> unsafeIOToPrim $ Storable.pokeByteOff backingArr (n * sizeElement @key @value) v)
    (\n v -> unsafeIOToPrim $ Storable.pokeByteOff backingArr (n * sizeElement @key @value + keyOffset @key) v)
    (\n v -> unsafeIOToPrim $ Storable.pokeByteOff backingArr (n * sizeElement @key @value + valueOffset @key @value) v)
    (\_ -> pure ())
    (\_ -> pure ())
    maxDistance (fromIntegral capacity) k hs

foldM
  :: forall z key value m
  . ( PrimMonad m, Storable.Storable key, Storable.Storable value )
  => HashTable (PrimState m) key value -> (z -> key -> value -> m z) -> m z -> m z
{-# INLINE foldM #-}
foldM HashTable_SS{..} f mz = do
  MutablePrimArray backingArr# <- primitive $ \s -> case readMutVar# backingRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  let backingArr = Ptr (mutableByteArrayContents# backingArr#)

  oldMaxDistance <- fromIntegral <$> readPrimVar maxDistanceRef
  capacity <- fromIntegral <$> readPrimVar capacityRef

  let go !n !z
        | n >= capacity + oldMaxDistance = pure z
        | otherwise = do
          distance :: Int8 <- unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value
          if distance == -1
            then go (n + 1) z
            else do
              key <- unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value + keyOffset @key
              val <- unsafeIOToPrim $ Storable.peekByteOff backingArr $ n * sizeElement @key @value + valueOffset @key @value
              f z key val >>= go (n + 1)
  mz >>= go 0

foreign import ccall unsafe "string.h" memset  :: Ptr a -> CInt  -> CSize -> IO (Ptr a)
