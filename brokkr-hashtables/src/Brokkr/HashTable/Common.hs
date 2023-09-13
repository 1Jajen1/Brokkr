{-# LANGUAGE MultiWayIf #-}
module Brokkr.HashTable.Common (
  lookupWithHash
, insertWithHash
, deleteWithHash
, nextPowerOf2
, maxDistanceFor
) where

import Control.Monad.Primitive

import Data.Bits
import Data.Int
import Data.Primitive (sizeOf)
import Control.Monad

nextPowerOf2 :: Int -> Int
{-# INLINE nextPowerOf2 #-}
nextPowerOf2 sz = if sz == 1 then 1 else 1 `unsafeShiftL` (8 * sizeOf (undefined :: Int) - countLeadingZeros (sz - 1))

-- Invariant: Arg is a power of 2
maxDistanceFor :: Int -> Int
{-# INLINE maxDistanceFor #-}
maxDistanceFor sz = countTrailingZeros sz

lookupWithHash
  :: (PrimMonad m, Eq key)
  => (Int -> m Int8)
  -> (Int -> m key)
  -> (Int -> m value)
  -> Int
  -> key
  -> Int
  -> (value -> m r)
  -> m r
  -> m r
{-# INLINE lookupWithHash #-}
lookupWithHash readDistance readKey readValue capacity !key !hs onSucc onFail = go 0
  where
    start = hs .&. (capacity - 1)
    go n = do
      let ind = start + fromIntegral n
      distance <- readDistance ind
      if distance < n
        then onFail
        else do
          key' <- readKey ind
          if key == key'
            then readValue ind >>= onSucc
            else go (n + 1)

insertWithHash
  :: forall key value m
  . (PrimMonad m, Eq key)
  => (key -> value -> m ())
  -> m Int
  -> (Int -> m Int8)
  -> (Int -> m key)
  -> (Int -> m value)
  -> (Int -> Int8 -> m ())
  -> (Int -> key -> m ())
  -> (Int -> value -> m ())
  -> Float
  -> Int8
  -> Int
  -> key
  -> Int
  -> value
  -> m ()
{-# INLINE insertWithHash #-}
insertWithHash
  onGrow
  incSize
  readDistance readKey readValue
  writeDistance writeKey writeValue
  !maxLoadFactor !maxDistance !capacity !k0 !hs = go start0 0 k0
  where
    start0 = hs .&. (capacity - 1)
    go :: Int -> Int8 -> key -> value -> m ()
    go ind n k v
      | n > maxDistance = onGrow k v
      | otherwise = do
        distance <- readDistance ind
        if | distance == (-1) -> do
              sz <- incSize

              writeDistance ind n
              writeKey ind k
              writeValue ind v

              let arrCap = fromIntegral $ fromIntegral maxDistance + capacity
              when (fromIntegral (sz + 1) / arrCap > maxLoadFactor) $ onGrow k v
            | distance < n -> do

              writeDistance ind n
              key <- readKey ind
              writeKey ind k
              val <- readValue ind
              writeValue ind v

              go (ind + 1) (distance + 1) key val
            | otherwise -> do
              key <- readKey ind
              if key == k
                then writeValue ind v
                else go (ind + 1) (n + 1) k v

deleteWithHash
  :: (PrimMonad m, Eq key)
  => m ()
  -> (Int -> m Int8)
  -> (Int -> m key)
  -> (Int -> m value)
  -> (Int -> Int8 -> m ())
  -> (Int -> key -> m ())
  -> (Int -> value -> m ())
  -> (Int -> m ())
  -> (Int -> m ())
  -> Int8
  -> Int
  -> key
  -> Int
  -> m (Maybe value)
{-# INLINE deleteWithHash #-}
deleteWithHash
  decSize
  readDistance readKey readValue
  writeDistance writeKey writeValue
  delKey delValue
  !maxDistance !capacity !k !hs = goDel 0
  where
    start = hs .&. (capacity - 1)
    goDel n = do
      let ind = start + fromIntegral n
      distance <- readDistance ind
      if distance < n
        then pure Nothing
        else do
          key <- readKey ind
          if key == k
            then do
              decSize

              writeDistance ind (-1)
              delKey ind
              val <- readValue ind
              delValue ind

              moveNext (n + 1)
              pure $ Just val
            else goDel (n + 1)
    moveNext n | n >= maxDistance = pure ()
    moveNext n = do
      let ind = start + fromIntegral n
      distance <- readDistance ind
      if distance < n
        then pure ()
        else do
          writeDistance ind (-1)
          writeDistance (ind - 1) (distance - 1)

          key <- readKey ind
          delKey ind
          writeKey (ind - 1) key

          val <- readValue ind
          delValue ind
          writeValue (ind - 1) val

          moveNext (n + 1)
