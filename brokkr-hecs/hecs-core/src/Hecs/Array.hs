{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Hecs.Array (
  Array
, new
, size
, writeBack
, read
, write
) where

import Prelude hiding (read)

import GHC.Exts
import GHC.IO

-- Growable array
data Array a = Array# Int# (SmallMutableArray# RealWorld a)

new :: forall a . Int -> IO (Array a)
new (I# initSz) = IO $ \s ->
  case newSmallArray# initSz (error "Hecs.Array.new:uninitlialized element") s of
    (# s', arr #) -> (# s', Array# 0# arr #)
{-# INLINE new #-}

size :: Array a -> Int
size (Array# sz _) = I# sz
{-# INLINE size #-}

grow :: Array a -> IO (Array a)
grow (Array# sz arr) = IO $ \s -> case newSmallArray# dCap (error "Hecs.Array.new:uninitlialized element") s of
  (# s1, arr1 #) -> case copySmallMutableArray# arr 0# arr1 0# cap s1 of
    s2 -> (# s2, Array# sz arr1 #)
  where
    cap = sizeofSmallMutableArray# arr
    dCap = cap *# 2#

read :: Array a -> Int -> IO a
read (Array# _ arr) (I# n) = IO (readSmallArray# arr n)
{-# INLINE read #-}

write :: Array a -> Int -> a -> IO ()
write (Array# _ arr) (I# n) el = IO $ \s -> case writeSmallArray# arr n el s of s1 -> (# s1, () #)
{-# INLINE write #-}

writeBack :: Array a -> a -> IO (Array a)
writeBack a@(Array# sz arr) el
  | isTrue# (sz >=# cap) = do
    Array# _ arr1 <- grow a
    IO $ \s -> case writeSmallArray# arr1 sz el s of s1 -> (# s1, Array# (sz +# 1#) arr1 #)
  | otherwise = IO $ \s -> case writeSmallArray# arr sz el s of s1 -> (# s1, Array# (sz +# 1#) arr #)
  where
    cap = sizeofSmallMutableArray# arr
{-# INLINE writeBack #-}
