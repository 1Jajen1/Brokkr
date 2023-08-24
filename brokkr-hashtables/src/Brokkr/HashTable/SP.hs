{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Brokkr.HashTable.SP (
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
import Brokkr.HashTable.Internal qualified as I
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

type HashTable s key value = HashTable' Storable I.Prim s key value

data instance HashTable' Storable I.Prim s key value =
  HashTable_SP {
    hashSalt       :: {-# UNPACK #-} !Salt
  , maxLoadFactor  :: {-# UNPACK #-} !MaxLoadFactor
  , sizeRef        :: {-# UNPACK #-} !(PrimVar s Int32) -- how many elements
  , capacityRef    :: {-# UNPACK #-} !(PrimVar s Int32) -- whats our capacity. The actual array size is this plus the max distance
  , maxDistanceRef :: {-# UNPACK #-} !(PrimVar s Int8) -- how many elements we can be away from the ideal spot before resizing
    -- Uses MutVar# to store an unlifted MutableArray# which otherwise would need to be boxed and allocated
  , backingDistAndKeyRef :: MutVar# s (MutableByteArray# s)
  , backingValRef  :: MutVar# s (MutableByteArray# s)
  }

new :: forall key value m . (PrimMonad m, Storable.Storable key, Prim value) => Salt -> MaxLoadFactor -> m (HashTable (PrimState m) key value)
{-# INLINE new #-}
new hashSalt maxLoadFactor = do
  sizeRef <- newPrimVar 0
  maxDistanceRef <- newPrimVar $ fromIntegral initMaxDistance
  capacityRef <- newPrimVar $ fromIntegral initCap
  MutableByteArray distArr <- newAlignedPinnedByteArray (initArrSz * elementSize) alignKey
  MutablePrimArray valArr <- if needsAlignment @value
    then newAlignedPinnedPrimArray @_ @value initArrSz
    else newPrimArray initArrSz
  t <- primitive $ \s -> case newMutVar# valArr s of
      (# s2, backingValRef #) -> case newMutVar# distArr s2 of
          (# s3, backingDistAndKeyRef #) -> (# s3, HashTable_SP{..} #)
  _ <- unsafeIOToPrim $ memset (Ptr (mutableByteArrayContents# distArr)) (-1) (fromIntegral $ initArrSz * elementSize)
  pure t
  where
    initCap = 32
    initMaxDistance = 5
    !initArrSz = initCap + initMaxDistance
    alignKey = Storable.alignment (undefined :: key)
    elementSize = sizeElement @key

-- Anything not 1,2,4,8 byte aligned needs a pinned aligned byte array!
needsAlignment :: forall key . Prim key => Bool
needsAlignment = case alignment (undefined :: key) of
  1 -> False
  2 -> False
  4 -> False
  8 -> False
  _ -> True

sizeElement :: forall key . Storable.Storable key => Int
sizeElement = sizeKey + alignKey + getEndPad (sizeKey + alignKey) alignKey
  where
    sizeKey = Storable.sizeOf (undefined :: key)
    alignKey = Storable.alignment (undefined :: key)
    getEndPad sz al =
      let extra = sz `rem` al
      in if extra == 0 then 0 else al


-- TODO This does not respect end padding!

size :: PrimMonad m => HashTable (PrimState m) key value -> m Int
size HashTable_SP{..} = fromIntegral <$> readPrimVar sizeRef

lookup :: (PrimMonad m, Eq key, Hash key, Storable.Storable key, Prim value) => HashTable (PrimState m) key value -> key -> m (Maybe value)
{-# INLINE lookup #-}
lookup ht key = lookupWithHash ht key (coerce (hash key) (hashSalt ht))

lookupWithHash :: forall key value m . (PrimMonad m, Eq key, Storable.Storable key, Prim value) => HashTable (PrimState m) key value -> key -> Int -> m (Maybe value)
{-# INLINE lookupWithHash #-}
lookupWithHash HashTable_SP{..} key !hs = do
  MutablePrimArray distAndKeyArr# <- primitive $ \s -> case readMutVar# backingDistAndKeyRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  let distAndKeyArr = Ptr (mutableByteArrayContents# distAndKeyArr#)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  capacity <- fromIntegral <$> readPrimVar capacityRef
  Common.lookupWithHash
    (\n -> unsafeIOToPrim $ Storable.peekByteOff distAndKeyArr (n * sizeElement @key))
    (\n -> unsafeIOToPrim $ Storable.peekByteOff distAndKeyArr (n * sizeElement @key + Storable.alignment (undefined :: key)))
    (readPrimArray valArr)
    capacity key hs

insert :: (PrimMonad m, Eq key, Hash key, Storable.Storable key, Prim value) => HashTable (PrimState m) key value -> key -> value -> m ()
{-# INLINE insert #-}
insert ht key = insertWithHash ht key (coerce (hash key) (hashSalt ht))

insertWithHash :: forall key value m . (PrimMonad m, Eq key, Hash key, Storable.Storable key, Prim value) => HashTable (PrimState m) key value -> key -> Int -> value -> m ()
{-# INLINEABLE insertWithHash #-}
insertWithHash ht@HashTable_SP{..} k0 !hs v0 = do
  MutablePrimArray distAndKeyArr# <- primitive $ \s -> case readMutVar# backingDistAndKeyRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  let distAndKeyArr = Ptr (mutableByteArrayContents# distAndKeyArr#)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  maxDistance <- readPrimVar maxDistanceRef
  capacity <- readPrimVar capacityRef
  Common.insertWithHash
    (\k v -> grow ht >> insert ht k v)
    (readPrimVar sizeRef >>= \sz -> writePrimVar sizeRef (sz + 1) >> pure (fromIntegral sz))
    (\n -> unsafeIOToPrim $ Storable.peekByteOff distAndKeyArr (n * sizeElement @key))
    (\n -> unsafeIOToPrim $ Storable.peekByteOff distAndKeyArr (n * sizeElement @key + Storable.alignment (undefined :: key)))
    (readPrimArray valArr)
    (\n v -> unsafeIOToPrim $ Storable.pokeByteOff distAndKeyArr (n * sizeElement @key) v)
    (\n v -> unsafeIOToPrim $ Storable.pokeByteOff distAndKeyArr (n * sizeElement @key + Storable.alignment (undefined :: key)) v)
    (writePrimArray valArr)
    maxLoadFactor maxDistance (fromIntegral capacity) k0 hs v0

grow :: (PrimMonad m, Eq key, Hash key, Storable.Storable key, Prim value) => HashTable (PrimState m) key value -> m ()
{-# INLINE grow #-}
grow ht@HashTable_SP{..} = do
  capacity <- fromIntegral <$> readPrimVar capacityRef
  growTo ht $ capacity * 2

growTo :: forall key value m . (PrimMonad m, Eq key, Hash key, Storable.Storable key, Prim value) => HashTable (PrimState m) key value -> Int -> m ()
{-# INLINEABLE growTo #-}
growTo ht@HashTable_SP{..} newSz = do
  MutablePrimArray distAndKeyArr# <- primitive $ \s -> case readMutVar# backingDistAndKeyRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  let distAndKeyArr = Ptr (mutableByteArrayContents# distAndKeyArr#)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)

  oldMaxDistance <- fromIntegral <$> readPrimVar maxDistanceRef
  capacity <- fromIntegral <$> readPrimVar capacityRef

  when (capacity < newSz) $ do
    let newArrSize = newSz + fromIntegral newMaxDistance
        newMaxDistance = fromIntegral $ countTrailingZeros newSz

    (MutableByteArray newDistAndKeyArr#) <- newAlignedPinnedByteArray (newArrSize * sizeElement @key) (Storable.alignment (undefined :: key))
    _ <- unsafeIOToPrim $ memset (Ptr (mutableByteArrayContents# newDistAndKeyArr#)) (-1) (fromIntegral $ newArrSize * sizeElement @key)
    (MutablePrimArray newValArr#) <- if needsAlignment @value
      then newAlignedPinnedPrimArray @_ @value newArrSize
      else newPrimArray newArrSize

    writePrimVar sizeRef 0
    writePrimVar maxDistanceRef newMaxDistance
    writePrimVar capacityRef (fromIntegral newSz)
    primitive $ \s -> (# writeMutVar# backingDistAndKeyRef newDistAndKeyArr# s, () #)
    primitive $ \s -> (# writeMutVar# backingValRef newValArr# s, () #)

    let go n
          | n >= capacity + oldMaxDistance = pure ()
          | otherwise = do
            distance :: Int8 <- unsafeIOToPrim $ Storable.peekByteOff distAndKeyArr (n * sizeElement @key)
            if distance == -1
              then go (n + 1)
              else do
                key <- unsafeIOToPrim $ Storable.peekByteOff distAndKeyArr (n * sizeElement @key + Storable.alignment (undefined :: key))
                val <- readPrimArray valArr n
                insert ht key val
                go (n + 1)
    go 0

reserve :: (PrimMonad m, Eq key, Hash key, Storable.Storable key, Prim value) => HashTable (PrimState m) key value -> Int -> m ()
{-# INLINE reserve #-}
reserve ht@HashTable_SP{..} sz0 = do
  capacity <- fromIntegral <$> readPrimVar capacityRef
  when (capacity < newSz) $ growTo ht newSz
  where
    sz :: Int = floor $ fromIntegral sz0 / maxLoadFactor
    newSz = if sz == 1 then 1 else 1 `unsafeShiftL` (8 * sizeOf (undefined :: Int) - countLeadingZeros (sz - 1))

delete :: (PrimMonad m, Eq key, Hash key, Storable.Storable key, Prim value) => HashTable (PrimState m) key value -> key -> m (Maybe value)
{-# INLINE delete #-}
delete ht k = deleteWithHash ht k (coerce (hash k) (hashSalt ht)) 

deleteWithHash :: forall key value m . (PrimMonad m, Eq key, Storable.Storable key, Prim value) => HashTable (PrimState m) key value -> key -> Int -> m (Maybe value)
{-# INLINE deleteWithHash #-}
deleteWithHash HashTable_SP{..} k !hs = do
  MutablePrimArray distAndKeyArr# <- primitive $ \s -> case readMutVar# backingDistAndKeyRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  let distAndKeyArr = Ptr (mutableByteArrayContents# distAndKeyArr#)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)

  capacity <- readPrimVar capacityRef
  maxDistance <- readPrimVar maxDistanceRef
  Common.deleteWithHash
    (readPrimVar sizeRef >>= \sz -> writePrimVar sizeRef (sz - 1))
    (\n -> unsafeIOToPrim $ Storable.peekByteOff distAndKeyArr (n * sizeElement @key))
    (\n -> unsafeIOToPrim $ Storable.peekByteOff distAndKeyArr ((n * 2 + 1) * Storable.sizeOf (undefined :: key)))
    (readPrimArray valArr)
    (\n v -> unsafeIOToPrim $ Storable.pokeByteOff distAndKeyArr (n * sizeElement @key) v)
    (\n v -> unsafeIOToPrim $ Storable.pokeByteOff distAndKeyArr (n * sizeElement @key + Storable.alignment (undefined :: key)) v)
    (writePrimArray valArr)
    (\_ -> pure ())
    (\_ -> pure ())
    maxDistance (fromIntegral capacity) k hs

foldM :: forall z key value m . (PrimMonad m, Storable.Storable key, Prim value) => HashTable (PrimState m) key value -> (z -> key -> value -> m z) -> m z -> m z
{-# INLINE foldM #-}
foldM HashTable_SP{..} f mz = do
  MutablePrimArray distAndKeyArr# <- primitive $ \s -> case readMutVar# backingDistAndKeyRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  let distAndKeyArr = Ptr (mutableByteArrayContents# distAndKeyArr#)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)

  oldMaxDistance <- fromIntegral <$> readPrimVar maxDistanceRef
  capacity <- fromIntegral <$> readPrimVar capacityRef

  let go !n !z
        | n >= capacity + oldMaxDistance = pure z
        | otherwise = do
          distance :: Int8 <- unsafeIOToPrim $ Storable.peekByteOff distAndKeyArr (n * sizeElement @key)
          if distance == -1
            then go (n + 1) z
            else do
              key <- unsafeIOToPrim $ Storable.peekByteOff distAndKeyArr (n * sizeElement @key + Storable.alignment (undefined :: key))
              val <- readPrimArray valArr n
              f z key val >>= go (n + 1)
  mz >>= go 0

foreign import ccall unsafe "string.h" memset  :: Ptr a -> CInt  -> CSize -> IO (Ptr a)
