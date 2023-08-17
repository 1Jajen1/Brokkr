{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE TypeFamilies #-}
module Hecs.HashTable.Int2Boxed (
  HashTable
, new
, insert
, lookup
, delete
) where

import Hecs.HashTable.HashKey

import Data.Bits
import Control.Monad
import Prelude hiding (lookup)
import Foreign.Storable
import Control.Monad.Primitive
import GHC.Exts
import GHC.IO hiding (liftIO)
import Control.Monad.IO.Class

-- Mutable linear hashtable
-- Used internally to map Components to their ids and will probably have some other uses later on
data HashTable value = HashTable {
  size    :: MutableByteArray# RealWorld
, capMask :: MutableByteArray# RealWorld
, backing :: MutVar# RealWorld (MutableArray# RealWorld (Entry value))
}

data Entry value :: UnliftedType where
  Empty :: Entry value
  Tomb  :: Entry value
  Entry :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> value -> Entry value

new :: forall value . Int -> IO (HashTable value)
new initSz = IO $ \s ->
  case newArray# iSzP2 Empty s of
    (# s1, arr #) -> case newMutVar# arr s1 of
      (# s2, arrRef #) -> case newByteArray# 8# s2 of
        (# s3, szRef #) -> case writeIntArray# szRef 0# 0# s3 of
          s4 -> case newByteArray# 8# s4 of
            (# s5, capMaskRef #) -> case writeIntArray# capMaskRef 0# (iSzP2 -# 1#) s5 of
              s6 -> (# s6, HashTable szRef capMaskRef arrRef #)
  where
    !(I# iSzP2) = if initSz == 1 then 1 else 1 `unsafeShiftL` (8 * sizeOf (undefined :: Int) - countLeadingZeros (initSz - 1))

insert :: forall value . HashTable value -> Int -> value -> IO ()
{-# INLINE insert #-}
insert old@HashTable{..} !k v = IO $ \s0 ->
  case readIntArray# capMask 0# s0 of
    (# s1, capMask# #) -> case readMutVar# backing s1 of
      (# s2, backing# #) ->
        case unsafeInsert capMask# backing# k (hash (hashKey k)) v of
            IO f -> case f s2 of
              (# s3, added #) -> case readIntArray# size 0# s3 of
                (# s4, sz# #) -> case (if added then writeIntArray# size 0# (sz# +# 1#) s4 else s4) of
                  s5 ->
                    let threeQuartersCap# = 3# *# ((capMask# +# 1#) `quotInt#` 4#)
                    in if isTrue# (sz# +# 1# >=# threeQuartersCap#)
                        then case resize old of IO g -> g s5
                        else (# s5, () #)

unsafeInsert :: forall value . Int# -> MutableArray# RealWorld (Entry value) -> Int -> Int -> value -> IO Bool
{-# INLINE unsafeInsert #-}
unsafeInsert capMask# backing# !k h@(I# h#) v = IO $ \s0 ->
  let start# = h# `andI#` capMask#
      go :: Int# -> State# RealWorld -> (# State# RealWorld, Bool #)
      go n s | isTrue# (n ># capMask#) = go 0# s
      go n s =
        case readArray# backing# n s of
          (# s', Entry k1 h1 _ #) -> if h1 == h && k1 == k
            then case writeArray# backing# n (Entry k h v) s' of s'' -> (# s'', False #)
            else go (n +# 1#) s'
          (# s', Tomb  #) -> go (n +# 1#) s'
          (# s', Empty #) -> case writeArray# backing# n (Entry k h v) s' of s'' -> (# s'', True #)
  in go start# s0

lookup :: forall value . HashTable value -> Int -> IO (Maybe value)
lookup HashTable{..} !k = liftIO $ IO $ \s0 ->
  case readIntArray# capMask 0# s0 of
    (# s1, capMask# #) -> case readMutVar# backing s1 of
      (# s2, backing# #) ->
        let !h@(I# h#) = hash (hashKey k)
            start# = h# `andI#` capMask#
            go :: Int# -> State# RealWorld -> (# State# RealWorld, Maybe value #)
            go n s | isTrue# (n ># capMask#) = go 0# s
            go n s =
              case readArray# backing# n s of
                (# s', Entry k1 h1 v1 #) -> if h1 == h && k1 == k then (# s', Just v1 #) else go (n +# 1#) s'
                (# s', Tomb #) -> go (n +# 1#) s'
                (# s', Empty #) -> (# s', Nothing #)
        in go start# s2

delete :: forall value . HashTable value -> Int -> IO (Maybe value)
delete HashTable{..} !k = IO $ \s0 ->
  case readIntArray# capMask 0# s0 of
    (# s1, capMask# #) ->
      case readIntArray# size 0# s1 of
        (# s2, sz# #) -> case readMutVar# backing s2 of
          (# s3, backing# #) ->
            let !h@(I# h#) = hash (hashKey k)
                start# = h# `andI#` capMask#
                go :: Int# -> State# RealWorld -> (# State# RealWorld, value, Bool #)
                go n s | isTrue# (n ># capMask#) = go 0# s
                go n s =
                  case readArray# backing# n s of
                    (# s', Entry k1 h1 v #) -> if h1 == h && k1 == k
                      then case writeArray# backing# n Tomb s' of s'' -> (# s'', v, True #)
                      else go (n +# 1#) s'
                    (# s', Tomb #) -> go (n +# 1#) s'
                    (# s', Empty #) -> (# s', undefined, False #)
            in case go start# s3 of
              (# s4, val, True #) -> (# writeIntArray# size 0# (sz# -# 1#) s4, Just val #)
              (# s4, _, False #)  -> (# s4, Nothing #)

iterateWithHash :: HashTable value -> (Int -> Int -> value -> IO ()) -> IO Int
iterateWithHash HashTable{..} f = IO $ \s0 ->
  case readIntArray# capMask 0# s0 of
    (# s1, capMask# #) -> case readMutVar# backing s1 of
      (# s2, backing# #) ->
        let go n acc s | isTrue# (n ># capMask#) = (# s, I# acc #)
            go n acc s =
              case readArray# backing# n s of
                (# s', Entry k1 h1 v1 #) -> case f k1 h1 v1 of IO f' -> case f' s' of (# s'', () #) -> go (n +# 1#) (acc +# 1#) s''
                (# s', _ #) -> go (n +# 1#) (acc +# 1#) s'
        in go 0# 0# s2

resize :: forall value . HashTable value -> IO ()
resize old@HashTable{..} = do
  IO $ \s ->
    case readIntArray# capMask 0# s of
      (# s0, capMask# #) ->
        let reqSz = (capMask# +# 1#) *# 2#
        in case newArray# reqSz Empty s0 of
          (# s1, arr #) -> case iterateWithHash old $ \k h v -> void $ unsafeInsert (reqSz -# 1#) arr k h v of
              IO f -> case f s1 of
                (# s2, I# newSz #) -> case writeMutVar# backing arr s2 of
                  s3 -> case writeIntArray# capMask 0# (reqSz -# 1#) s3 of
                    s4 -> (# writeIntArray# size 0# newSz s4, () #) 
  
