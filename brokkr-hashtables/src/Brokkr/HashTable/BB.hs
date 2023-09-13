{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
module Brokkr.HashTable.BB (
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

import Data.Int

import Data.Primitive
import Data.Primitive.PrimVar

import GHC.Exts
import Control.Monad (when)
import Foreign.C.Types (CInt(..), CSize(..))

-- TODO:
-- - Try to get more common code out. Especially the storable and prim utils
-- - Instances for HashTable' (Eq, Lift) would be nice

type HashTable s key value = HashTable' Boxed Boxed s key value

data instance HashTable' Boxed Boxed s key value =
  HashTable_BB {
    hashSalt       :: {-# UNPACK #-} !Salt
  , maxLoadFactor  :: {-# UNPACK #-} !MaxLoadFactor
  , sizeRef        :: {-# UNPACK #-} !(PrimVar s Int32) -- how many elements
  , capacityRef    :: {-# UNPACK #-} !(PrimVar s Int32) -- whats our capacity. The actual array size is this plus the max distance
  , maxDistanceRef :: {-# UNPACK #-} !(PrimVar s Int8) -- how many elements we can be away from the ideal spot before resizing
    -- Uses MutVar# to store an unlifted MutableArray# which otherwise would need to be boxed and allocated
  , backingDistRef :: MutVar# s (MutableByteArray# s)
  , backingKeysRef :: MutVar# s (MutableArray# s key)
  , backingValRef  :: MutVar# s (MutableArray# s value)
  }

instance Eq (HashTable s k v) where
  HashTable_BB{sizeRef = szRefL} == HashTable_BB{sizeRef = szRefR} = szRefL == szRefR

new :: PrimMonad m => Int -> Salt -> MaxLoadFactor -> m (HashTable (PrimState m) key value)
{-# INLINE new #-}
new initCap0 hashSalt maxLoadFactor = do
  sizeRef <- newPrimVar 0
  maxDistanceRef <- newPrimVar $ fromIntegral initMaxDistance
  capacityRef <- newPrimVar $ fromIntegral initCap
  MutableByteArray distArr <- newByteArray (I# initArrSz)
  t <- primitive $ \s -> case newArray# initArrSz (error "val:empty") s of
    (# s1, valArr #) -> case newMutVar# valArr s1 of
      (# s2, backingValRef #) -> case newArray# initArrSz (error "key:empty") s2 of
        (# s3, keyArr #) -> case newMutVar# keyArr s3 of
          (# s4, backingKeysRef #) -> case newMutVar# distArr s4 of
              (# s5, backingDistRef #) -> (# s5, HashTable_BB{..} #)
  _ <- unsafeIOToPrim $ memset (Ptr (mutableByteArrayContents# distArr)) (-1) (fromIntegral $ I# initArrSz)
  pure t
  where
    initCap1 = floor $ fromIntegral initCap0 / maxLoadFactor
    initCap = Common.nextPowerOf2 initCap1
    initMaxDistance = Common.maxDistanceFor initCap
    !(I# initArrSz) = initCap + initMaxDistance

size :: PrimMonad m => HashTable (PrimState m) key value -> m Int
size HashTable_BB{..} = fromIntegral <$> readPrimVar sizeRef

lookup :: (PrimMonad m, Eq key, Hash key) => HashTable (PrimState m) key value -> key -> (value -> m r) -> m r -> m r
{-# INLINE lookup #-}
lookup ht key onSucc onFail = lookupWithHash ht key (coerce (hash key) (hashSalt ht)) onSucc onFail

lookupWithHash :: (PrimMonad m, Eq key) => HashTable (PrimState m) key value -> key -> Int -> (value -> m r) -> m r -> m r
{-# INLINE lookupWithHash #-}
lookupWithHash HashTable_BB{..} key !hs onSucc onFail = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeysRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  capacity <- fromIntegral <$> readPrimVar capacityRef
  Common.lookupWithHash
    (readPrimArray @Int8 distArr)
    (readArray keyArr)
    (readArray valArr)
    capacity key hs onSucc onFail

insert :: (PrimMonad m, Eq key, Hash key) => HashTable (PrimState m) key value -> key -> value -> m ()
{-# INLINE insert #-}
insert ht key = insertWithHash ht key (coerce (hash key) (hashSalt ht))

insertWithHash :: (PrimMonad m, Eq key, Hash key) => HashTable (PrimState m) key value -> key -> Int -> value -> m ()
{-# INLINEABLE insertWithHash #-}
insertWithHash ht@HashTable_BB{..} k0 !hs v0 = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeysRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  maxDistance <- readPrimVar maxDistanceRef
  capacity <- readPrimVar capacityRef
  Common.insertWithHash
    (\k v -> grow ht >> insert ht k v)
    (readPrimVar sizeRef >>= \sz -> writePrimVar sizeRef (sz + 1) >> pure (fromIntegral sz))
    (readPrimArray @Int8 distArr)
    (readArray keyArr)
    (readArray valArr)
    (writePrimArray @Int8 distArr)
    (writeArray keyArr)
    (writeArray valArr)
    maxLoadFactor maxDistance (fromIntegral capacity) k0 hs v0

grow :: (PrimMonad m, Eq key, Hash key) => HashTable (PrimState m) key value -> m ()
{-# INLINE grow #-}
grow ht@HashTable_BB{..} = do
  capacity <- fromIntegral <$> readPrimVar capacityRef
  growTo ht $ capacity * 2

growTo :: (PrimMonad m, Eq key, Hash key) => HashTable (PrimState m) key value -> Int -> m ()
{-# INLINEABLE growTo #-}
growTo ht@HashTable_BB{..} newSz = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeysRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutableArray arr #)

  oldMaxDistance <- fromIntegral <$> readPrimVar maxDistanceRef
  capacity <- fromIntegral <$> readPrimVar capacityRef

  when (capacity < newSz) $ do
    let newArrSize = newSz + fromIntegral newMaxDistance
        newMaxDistance = fromIntegral $ Common.maxDistanceFor newSz

    (MutablePrimArray newDistArr#) <- newPrimArray @_ @Int8 newArrSize
    _ <- unsafeIOToPrim $ memset (Ptr (mutableByteArrayContents# newDistArr#)) (-1) (fromIntegral newArrSize)
    (MutableArray newKeyArr#) <- newArray newArrSize (error "key:empty")
    (MutableArray newValArr#) <- newArray newArrSize (error "val:empty")
    
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
                val <- readArray valArr n
                insert ht key val
                go (n + 1)
    go 0

reserve :: (PrimMonad m, Eq key, Hash key) => HashTable (PrimState m) key value -> Int -> m ()
{-# INLINE reserve #-}
reserve ht@HashTable_BB{..} sz0 = do
  capacity <- fromIntegral <$> readPrimVar capacityRef
  when (capacity < newSz) $ growTo ht newSz
  where
    sz :: Int = floor $ fromIntegral sz0 / maxLoadFactor
    newSz = Common.nextPowerOf2 sz

delete :: (PrimMonad m, Eq key, Hash key) => HashTable (PrimState m) key value -> key -> m (Maybe value)
{-# INLINE delete #-}
delete ht k = deleteWithHash ht k (coerce (hash k) (hashSalt ht)) 

deleteWithHash :: (PrimMonad m, Eq key) => HashTable (PrimState m) key value -> key -> Int -> m (Maybe value)
{-# INLINE deleteWithHash #-}
deleteWithHash HashTable_BB{..} k !hs = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeysRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
  valArr <- primitive $ \s -> case readMutVar# backingValRef s of (# s1, arr #) -> (# s1, MutableArray arr #)

  capacity <- readPrimVar capacityRef
  maxDistance <- readPrimVar maxDistanceRef
  Common.deleteWithHash
    (readPrimVar sizeRef >>= \sz -> writePrimVar sizeRef (sz - 1))
    (readPrimArray @Int8 distArr)
    (readArray keyArr)
    (readArray valArr)
    (writePrimArray @Int8 distArr)
    (writeArray keyArr)
    (writeArray valArr)
    (\n -> writeArray keyArr n $ error "key:empty")
    (\n -> writeArray valArr n $ error "val:empty")
    maxDistance (fromIntegral capacity) k hs

foldM :: PrimMonad m => HashTable (PrimState m) key value -> (z -> key -> value -> m z) -> m z -> m z
{-# INLINE foldM #-}
foldM HashTable_BB{..} f mz = do
  distArr <- primitive $ \s -> case readMutVar# backingDistRef s of (# s1, arr #) -> (# s1, MutablePrimArray arr #)
  keyArr <- primitive $ \s -> case readMutVar# backingKeysRef s of (# s1, arr #) -> (# s1, MutableArray arr #)
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
              key <- readArray keyArr n
              val <- readArray valArr n
              f z key val >>= go (n + 1)
  mz >>= go 0

foreign import ccall unsafe "string.h" memset  :: Ptr a -> CInt  -> CSize -> IO (Ptr a)
