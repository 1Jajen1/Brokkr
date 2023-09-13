{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Brokkr.HashTable.PB (
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

type HashTable s key value = HashTable' I.Prim Boxed s key value

-- TODO
-- - Verify alignment and element sizes for different keys

data instance HashTable' I.Prim Boxed s key value =
  HashTable_PB {
    hashSalt       :: {-# UNPACK #-} !Salt
  , maxLoadFactor  :: {-# UNPACK #-} !MaxLoadFactor
  , sizeRef        :: {-# UNPACK #-} !(PrimVar s Int32) -- how many elements
  , capacityRef    :: {-# UNPACK #-} !(PrimVar s Int32) -- whats our capacity. The actual array size is this plus the max distance
  , maxDistanceRef :: {-# UNPACK #-} !(PrimVar s Int8) -- how many elements we can be away from the ideal spot before resizing
    -- Uses MutVar# to store an unlifted MutableArray# which otherwise would need to be boxed and allocated
  , backingDistRef :: MutVar# s (MutableByteArray# s)
  , backingKeyRef :: MutVar# s (MutableByteArray# s)
  , backingValRef  :: MutVar# s (MutableArray# s value)
  }

instance Eq (HashTable s k v) where
  HashTable_PB{sizeRef = szRefL} == HashTable_PB{sizeRef = szRefR} = szRefL == szRefR

new :: forall key value m . (PrimMonad m, Prim key) => Int -> Salt -> MaxLoadFactor -> m (HashTable (PrimState m) key value)
{-# INLINE new #-}
new initCap0 hashSalt maxLoadFactor = do
  sizeRef <- newPrimVar 0
  maxDistanceRef <- newPrimVar $ fromIntegral initMaxDistance
  capacityRef <- newPrimVar $ fromIntegral initCap
  MutableByteArray distArr <- newByteArray initArrSz
  _ <- unsafeIOToPrim $ memset (Ptr (mutableByteArrayContents# distArr)) (-1) (fromIntegral initArrSz)
  MutablePrimArray keyArr <- if needsAlignment @key
    then newAlignedPinnedPrimArray @_ @key initArrSz
    else newPrimArray initArrSz
  MutableArray valArr <- newArray initArrSz (error "val:empty")
  primitive $ \s -> case newMutVar# valArr s of
    (# s1, backingValRef #) -> case newMutVar# keyArr s1 of
        (# s2, backingKeyRef #) -> case newMutVar# distArr s2 of
          (# s3, backingDistRef #) -> (# s3, HashTable_PB{..} #)
  where
    initCap1 = floor $ fromIntegral initCap0 / maxLoadFactor
    initCap = Common.nextPowerOf2 initCap1
    initMaxDistance = Common.maxDistanceFor initCap
    !initArrSz = initCap + initMaxDistance

needsAlignment :: forall val . Prim val => Bool
needsAlignment = alWord `rem` alEl /= 0
  where
    alEl = alignment (undefined :: val)
    alWord = alignment (undefined :: Int)

size :: PrimMonad m => HashTable (PrimState m) key value -> m Int
size HashTable_PB{..} = fromIntegral <$> readPrimVar sizeRef

lookup :: (PrimMonad m, Eq key, Hash key, Prim key) => HashTable (PrimState m) key value -> key -> (value -> m r) -> m r -> m r
{-# INLINE lookup #-}
lookup ht key onSucc onFail = lookupWithHash ht key (coerce (hash key) (hashSalt ht)) onSucc onFail

lookupWithHash :: forall key value m r . (PrimMonad m, Eq key, Prim key) => HashTable (PrimState m) key value -> key -> Int -> (value -> m r) -> m r -> m r
{-# INLINE lookupWithHash #-}
lookupWithHash HashTable_PB{..} key !hs onSucc onFail = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeyRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  capacity <- fromIntegral <$> readPrimVar capacityRef
  Common.lookupWithHash
    (readPrimArray distArr)
    (readPrimArray keyArr)
    (readArray valArr)
    capacity key hs onSucc onFail

insert :: (PrimMonad m, Eq key, Hash key, Prim key) => HashTable (PrimState m) key value -> key -> value -> m ()
{-# INLINE insert #-}
insert ht key = insertWithHash ht key (coerce (hash key) (hashSalt ht))

insertWithHash :: forall key value m . (PrimMonad m, Eq key, Hash key, Prim key) => HashTable (PrimState m) key value -> key -> Int -> value -> m ()
{-# INLINEABLE insertWithHash #-}
insertWithHash ht@HashTable_PB{..} k0 !hs v0 = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeyRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  maxDistance <- readPrimVar maxDistanceRef
  capacity <- readPrimVar capacityRef
  Common.insertWithHash
    (\k v -> grow ht >> insert ht k v)
    (readPrimVar sizeRef >>= \sz -> writePrimVar sizeRef (sz + 1) >> pure (fromIntegral sz))
    (readPrimArray distArr)
    (readPrimArray keyArr)
    (readArray valArr)
    (writePrimArray distArr)
    (writePrimArray keyArr)
    (writeArray valArr)
    maxLoadFactor maxDistance (fromIntegral capacity) k0 hs v0

grow :: (PrimMonad m, Eq key, Hash key, Prim key) => HashTable (PrimState m) key value -> m ()
{-# INLINE grow #-}
grow ht@HashTable_PB{..} = do
  capacity <- fromIntegral <$> readPrimVar capacityRef
  growTo ht $ capacity * 2

growTo :: forall key value m . (PrimMonad m, Eq key, Hash key, Prim key) => HashTable (PrimState m) key value -> Int -> m ()
{-# INLINEABLE growTo #-}
growTo ht@HashTable_PB{..} newSz = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeyRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutableArray arr #)

  oldMaxDistance <- fromIntegral <$> readPrimVar maxDistanceRef
  capacity <- fromIntegral <$> readPrimVar capacityRef

  when (capacity < newSz) $ do
    let newArrSize = newSz + fromIntegral newMaxDistance
        newMaxDistance = fromIntegral $ countTrailingZeros newSz

    MutablePrimArray newDistArr <- newPrimArray @_ @Int8 newArrSize
    _ <- unsafeIOToPrim $ memset (Ptr (mutableByteArrayContents# newDistArr)) (-1) (fromIntegral newArrSize)
    MutablePrimArray newKeyArr <- if needsAlignment @key
      then newAlignedPinnedPrimArray @_ @key newArrSize
      else newPrimArray newArrSize
    (MutableArray newValArr#) <- newArray newArrSize (error "val:empty")

    writePrimVar sizeRef 0
    writePrimVar maxDistanceRef newMaxDistance
    writePrimVar capacityRef (fromIntegral newSz)
    primitive $ \s -> (# writeMutVar# backingDistRef newDistArr s, () #)
    primitive $ \s -> (# writeMutVar# backingKeyRef newKeyArr s, () #)
    primitive $ \s -> (# writeMutVar# backingValRef newValArr# s, () #)

    let go n
          | n >= capacity + oldMaxDistance = pure ()
          | otherwise = do
            distance :: Int8 <- readPrimArray distArr n
            if distance == -1
              then go (n + 1)
              else do
                key <- readPrimArray keyArr n
                val <- readArray valArr n
                insert ht key val
                go (n + 1)
    go 0

reserve :: (PrimMonad m, Eq key, Hash key, Prim key) => HashTable (PrimState m) key value -> Int -> m ()
{-# INLINE reserve #-}
reserve ht@HashTable_PB{..} sz0 = do
  capacity <- fromIntegral <$> readPrimVar capacityRef
  when (capacity < newSz) $ growTo ht newSz
  where
    sz :: Int = floor $ fromIntegral sz0 / maxLoadFactor
    newSz = if sz == 1 then 1 else 1 `unsafeShiftL` (8 * sizeOf (undefined :: Int) - countLeadingZeros (sz - 1))

delete :: (PrimMonad m, Eq key, Hash key, Prim key) => HashTable (PrimState m) key value -> key -> m (Maybe value)
{-# INLINE delete #-}
delete ht k = deleteWithHash ht k (coerce (hash k) (hashSalt ht)) 

deleteWithHash :: forall key value m . (PrimMonad m, Eq key, Prim key) => HashTable (PrimState m) key value -> key -> Int -> m (Maybe value)
{-# INLINE deleteWithHash #-}
deleteWithHash HashTable_PB{..} k !hs = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeyRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutableArray arr #)

  capacity <- readPrimVar capacityRef
  maxDistance <- readPrimVar maxDistanceRef
  Common.deleteWithHash
    (readPrimVar sizeRef >>= \sz -> writePrimVar sizeRef (sz - 1))
    (readPrimArray distArr)
    (readPrimArray keyArr)
    (readArray valArr)
    (writePrimArray distArr)
    (writePrimArray keyArr)
    (writeArray valArr)
    (\_ -> pure ())
    (\n -> writeArray valArr n $ error "val:empty")
    maxDistance (fromIntegral capacity) k hs

foldM :: forall z key value m . (PrimMonad m, Prim key) => HashTable (PrimState m) key value -> (z -> key -> value -> m z) -> m z -> m z
{-# INLINE foldM #-}
foldM HashTable_PB{..} f mz = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeyRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutableArray arr #)

  oldMaxDistance <- fromIntegral <$> readPrimVar maxDistanceRef
  capacity <- fromIntegral <$> readPrimVar capacityRef

  let go !n !z
        | n >= capacity + oldMaxDistance = pure z
        | otherwise = do
          distance :: Int8 <- readPrimArray distArr n
          if distance == -1
            then go (n + 1) z
            else do
              key <- readPrimArray keyArr n
              val <- readArray valArr n
              f z key val >>= go (n + 1)
  mz >>= go 0

foreign import ccall unsafe "string.h" memset  :: Ptr a -> CInt  -> CSize -> IO (Ptr a)
