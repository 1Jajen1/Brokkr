{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Brokkr.HashTable.BP (
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

type HashTable s key value = HashTable' Boxed I.Prim s key value

data instance HashTable' Boxed I.Prim s key value =
  HashTable_BP {
    hashSalt       :: {-# UNPACK #-} !Salt
  , maxLoadFactor  :: {-# UNPACK #-} !MaxLoadFactor
  , sizeRef        :: {-# UNPACK #-} !(PrimVar s Int32) -- how many elements
  , capacityRef    :: {-# UNPACK #-} !(PrimVar s Int32) -- whats our capacity. The actual array size is this plus the max distance
  , maxDistanceRef :: {-# UNPACK #-} !(PrimVar s Int8) -- how many elements we can be away from the ideal spot before resizing
    -- Uses MutVar# to store an unlifted MutableArray# which otherwise would need to be boxed and allocated
  , backingDistRef :: MutVar# s (MutableByteArray# s)
  , backingKeysRef :: MutVar# s (MutableArray# s key)
  , backingValRef  :: MutVar# s (MutableByteArray# s)
  }

instance Eq (HashTable s k v) where
  HashTable_BP{sizeRef = szRefL} == HashTable_BP{sizeRef = szRefR} = szRefL == szRefR

new :: forall key value m . (PrimMonad m, Prim value) => Int -> Salt -> MaxLoadFactor -> m (HashTable (PrimState m) key value)
{-# INLINE new #-}
new initCap0 hashSalt maxLoadFactor = do
  sizeRef <- newPrimVar 0
  maxDistanceRef <- newPrimVar $ fromIntegral initMaxDistance
  capacityRef <- newPrimVar $ fromIntegral initCap
  MutableByteArray distArr <- newByteArray initArrSz
  MutableArray keyArr <- newArray initArrSz (error "key:empty")
  MutablePrimArray valArr <- if needsAlignment @value
    then newAlignedPinnedPrimArray initArrSz
    else newPrimArray @_ @value initArrSz
  t <- primitive $ \s -> case newMutVar# valArr s of
    (# s1, backingValRef #) -> case newMutVar# keyArr s1 of
      (# s2, backingKeysRef #) -> case newMutVar# distArr s2 of
          (# s3, backingDistRef #) -> (# s3, HashTable_BP{..} #)
  _ <- unsafeIOToPrim $ memset (Ptr (mutableByteArrayContents# distArr)) (-1) (fromIntegral initArrSz)
  pure t
  where
    initCap1 = floor $ fromIntegral initCap0 / maxLoadFactor
    initCap = Common.nextPowerOf2 initCap1
    initMaxDistance = Common.maxDistanceFor initCap
    initArrSz = initCap + initMaxDistance

needsAlignment :: forall val . Prim val => Bool
needsAlignment = alWord `rem` alEl /= 0
  where
    alEl = alignment (undefined :: val)
    alWord = alignment (undefined :: Int)

size :: PrimMonad m => HashTable (PrimState m) key value -> m Int
size HashTable_BP{..} = fromIntegral <$> readPrimVar sizeRef

lookup :: (PrimMonad m, Eq key, Hash key, Prim value) => HashTable (PrimState m) key value -> key -> m (Maybe value)
{-# INLINE lookup #-}
lookup ht key = lookupWithHash ht key (coerce (hash key) (hashSalt ht))

lookupWithHash :: forall key value m . (PrimMonad m, Eq key, Prim value) => HashTable (PrimState m) key value -> key -> Int -> m (Maybe value)
{-# INLINEABLE lookupWithHash #-}
lookupWithHash HashTable_BP{..} key !hs = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeysRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  capacity <- fromIntegral <$> readPrimVar capacityRef
  Common.lookupWithHash
    (readPrimArray @Int8 distArr)
    (readArray keyArr)
    (readPrimArray valArr)
    capacity key hs

insert :: (PrimMonad m, Eq key, Hash key, Prim value) => HashTable (PrimState m) key value -> key -> value -> m ()
{-# INLINE insert #-}
insert ht key = insertWithHash ht key (coerce (hash key) (hashSalt ht))

insertWithHash :: forall key value m . (PrimMonad m, Eq key, Hash key, Prim value) => HashTable (PrimState m) key value -> key -> Int -> value -> m ()
{-# INLINEABLE insertWithHash #-}
insertWithHash ht@HashTable_BP{..} k0 !hs v0 = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeysRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  maxDistance <- readPrimVar maxDistanceRef
  capacity <- readPrimVar capacityRef
  Common.insertWithHash
    (\k v -> grow ht >> insert ht k v)
    (readPrimVar sizeRef >>= \sz -> writePrimVar sizeRef (sz + 1) >> pure (fromIntegral sz))
    (readPrimArray @Int8 distArr)
    (readArray keyArr)
    (readPrimArray valArr)
    (writePrimArray @Int8 distArr)
    (writeArray keyArr)
    (writePrimArray valArr)
    maxLoadFactor maxDistance (fromIntegral capacity) k0 hs v0

grow :: (PrimMonad m, Eq key, Hash key, Prim value) => HashTable (PrimState m) key value -> m ()
{-# INLINE grow #-}
grow ht@HashTable_BP{..} = do
  capacity <- fromIntegral <$> readPrimVar capacityRef
  growTo ht $ capacity * 2

growTo :: forall key value m . (PrimMonad m, Eq key, Hash key, Prim value) => HashTable (PrimState m) key value -> Int -> m ()
{-# INLINEABLE growTo #-}
growTo ht@HashTable_BP{..} newSz = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeysRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)

  oldMaxDistance <- fromIntegral <$> readPrimVar maxDistanceRef
  capacity <- fromIntegral <$> readPrimVar capacityRef

  when (capacity < newSz) $ do
    let newArrSize = newSz + fromIntegral newMaxDistance
        newMaxDistance = fromIntegral $ countTrailingZeros newSz

    MutablePrimArray newDistArr# <- newPrimArray @_ @Int8 newArrSize
    _ <- unsafeIOToPrim $ memset (Ptr (mutableByteArrayContents# newDistArr#)) (-1) (fromIntegral newArrSize)
    MutableArray newKeyArr# <- newArray newArrSize (error "key:empty")
    MutablePrimArray newValArr# <- if needsAlignment @value
      then newAlignedPinnedPrimArray newArrSize
      else newPrimArray @_ @value newArrSize

    writePrimVar sizeRef 0
    writePrimVar maxDistanceRef newMaxDistance
    writePrimVar capacityRef (fromIntegral newSz)
    primitive $ \s -> (# writeMutVar# backingDistRef newDistArr# s, () #)
    primitive $ \s -> (# writeMutVar# backingKeysRef newKeyArr# s, () #)
    primitive $ \s -> (# writeMutVar# backingValRef newValArr# s, () #)

    let go n
          | n >= capacity + oldMaxDistance = pure ()
          | otherwise = do
            distance :: Int8 <- readPrimArray distArr n
            if distance == -1
              then go (n + 1)
              else do
                key <- readArray keyArr n
                val <- readPrimArray valArr n
                insert ht key val
                go (n + 1)
    go 0

reserve :: (PrimMonad m, Eq key, Hash key, Prim value) => HashTable (PrimState m) key value -> Int -> m ()
{-# INLINE reserve #-}
reserve ht@HashTable_BP{..} sz0 = do
  capacity <- fromIntegral <$> readPrimVar capacityRef
  when (capacity < newSz) $ growTo ht newSz
  where
    sz :: Int = floor $ fromIntegral sz0 / maxLoadFactor
    newSz = if sz == 1 then 1 else 1 `unsafeShiftL` (8 * sizeOf (undefined :: Int) - countLeadingZeros (sz - 1))

delete :: (PrimMonad m, Eq key, Hash key, Prim value) => HashTable (PrimState m) key value -> key -> m (Maybe value)
{-# INLINE delete #-}
delete ht k = deleteWithHash ht k (coerce (hash k) (hashSalt ht)) 

deleteWithHash :: forall key value m . (PrimMonad m, Eq key, Prim value) => HashTable (PrimState m) key value -> key -> Int -> m (Maybe value)
{-# INLINE deleteWithHash #-}
deleteWithHash HashTable_BP{..} k !hs = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeysRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)

  capacity <- readPrimVar capacityRef
  maxDistance <- readPrimVar maxDistanceRef
  Common.deleteWithHash
    (readPrimVar sizeRef >>= \sz -> writePrimVar sizeRef (sz - 1))
    (readPrimArray @Int8 distArr)
    (readArray keyArr)
    (readPrimArray valArr)
    (writePrimArray @Int8 distArr)
    (writeArray keyArr)
    (writePrimArray valArr)
    (\n -> writeArray keyArr n $ error "key:empty")
    (\_ -> pure ())
    maxDistance (fromIntegral capacity) k hs

foldM :: forall key value m z . (PrimMonad m, Prim value) => HashTable (PrimState m) key value -> (z -> key -> value -> m z) -> m z -> m z
{-# INLINE foldM #-}
foldM HashTable_BP{..} f mz = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeysRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)

  oldMaxDistance <- fromIntegral <$> readPrimVar maxDistanceRef
  capacity <- fromIntegral <$> readPrimVar capacityRef

  let go !n !z
        | n >= capacity + oldMaxDistance = pure z
        | otherwise = do
          distance :: Int8 <- readPrimArray distArr n
          if distance == -1
            then go (n + 1) z
            else do
              key <- readArray keyArr n
              val <- readPrimArray valArr n
              f z key val >>= go (n + 1)
  mz >>= go 0

foreign import ccall unsafe "string.h" memset  :: Ptr a -> CInt  -> CSize -> IO (Ptr a)
