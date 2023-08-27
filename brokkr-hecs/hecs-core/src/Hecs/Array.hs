{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Hecs.Array (
  Array
, new
, size
, writeBack
, read
, write
, iterate_
) where

import Prelude hiding (read)

import Control.Monad.Primitive

import Data.Primitive hiding (Array)
import Data.Primitive.PrimVar

import GHC.Exts

-- Growable array
data Array s a = Array {-# UNPACK #-} !(PrimVar s Int) (MutVar# s (SmallMutableArray# s a))

new :: PrimMonad m => Int -> m (Array (PrimState m) a)
{-# INLINEABLE new #-}
new initSz = do
  szRef <- newPrimVar 0
  SmallMutableArray arr <- newSmallArray initSz (error "arr:new:empty")
  primitive $ \s -> case newMutVar# arr s of
    (# s1, arrRef #) -> (# s1, Array szRef arrRef #)

size :: PrimMonad m => Array (PrimState m) a -> m Int
{-# INLINE size #-}
size (Array sz _) = readPrimVar sz

grow :: PrimMonad m => Array (PrimState m) a -> m ()
{-# INLINEABLE grow #-}
grow (Array szRef arrRef) = do
  arr <- primitive $ \s -> case readMutVar# arrRef s of (# s1, arr #) -> (# s1, SmallMutableArray arr #)

  let cap = sizeofSmallMutableArray arr
  let newCap = cap * 2

  newArr@(SmallMutableArray newArr#) <- newSmallArray newCap (error "arr:grow:empty")
  copySmallMutableArray newArr 0 arr 0 cap
  
  primitive $ \s -> (# writeMutVar# arrRef newArr# s, () #)

read :: PrimMonad m => Array (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read (Array _ arrRef) !n = do
  arr <- primitive $ \s -> case readMutVar# arrRef s of (# s1, arr #) -> (# s1, SmallMutableArray arr #)
  readSmallArray arr n

write :: PrimMonad m => Array (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write (Array _ arrRef) !n el = do
  arr <- primitive $ \s -> case readMutVar# arrRef s of (# s1, arr #) -> (# s1, SmallMutableArray arr #)
  writeSmallArray arr n el

writeBack :: PrimMonad m => Array (PrimState m) a -> a -> m ()
{-# INLINEABLE writeBack #-}
writeBack a@(Array szRef arrRef) el = do
  arr <- primitive $ \s -> case readMutVar# arrRef s of (# s1, arr #) -> (# s1, SmallMutableArray arr #)
  let cap = sizeofSmallMutableArray arr
  sz <- readPrimVar szRef

  if sz == cap
    then grow a >> write a sz el
    else writeSmallArray arr sz el
  writePrimVar szRef (sz + 1)

iterate_ :: PrimMonad m => Array (PrimState m) a -> (a -> m ()) -> m ()
{-# INLINE iterate_ #-}
iterate_ (Array szRef arrRef) hdl = do
  arr <- primitive $ \s -> case readMutVar# arrRef s of (# s1, arr #) -> (# s1, SmallMutableArray arr #)
  sz <- readPrimVar szRef
  let go !n
        | n >= sz = pure ()
        | otherwise = do
          el <- readSmallArray arr n
          hdl el
          go (n + 1)
  go 0
