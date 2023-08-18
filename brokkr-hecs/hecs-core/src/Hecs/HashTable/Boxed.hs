{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE TypeFamilies #-}
module Hecs.HashTable.Boxed (
  HashTable(..)
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

-- TODO Tombs are no overwriteable, that is unsafe if the element is still in!
-- TODO After benchmarks implement robin hood probing with fixed length probes?
-- TODO Benchmark thoroughly before embarking on any changes from now on

-- Mutable linear hashtable
-- Used internally to map Components to their ids and will probably have some other uses later on
data HashTable key value = HashTable {
  -- TODO Merge this into one array with 2 elements
  sizeCapMask :: MutableByteArray# RealWorld
, backing :: MutVar# RealWorld (MutableArray# RealWorld (Entry key value))
}

data Entry key value :: UnliftedType where
  Empty :: Entry key value
  Tomb  :: Entry key value
  Entry :: !key -> !Int -> value -> Entry key value

new :: forall key value . Int -> IO (HashTable key value)
new initSz = IO $ \s ->
  case newArray# iSzP2 Empty s of
    (# s1, arr #) -> case newMutVar# arr s1 of
      (# s2, arrRef #) -> case newByteArray# 16# s2 of
        (# s3, sizeCapMask #) -> case writeIntArray# sizeCapMask 0# 0# s3 of
          s4 -> case writeIntArray# sizeCapMask 1# (iSzP2 -# 1#) s4 of
              s5 -> (# s5, HashTable sizeCapMask arrRef #)
  where
    !(I# iSzP2) = if initSz == 1 then 1 else 1 `unsafeShiftL` (8 * sizeOf (undefined :: Int) - countLeadingZeros (initSz - 1))

insert :: forall key value . HashKey key => HashTable key value -> key -> value -> IO ()
{-# INLINE insert #-}
insert old@HashTable{..} !k v = IO $ \s0 ->
  case readIntArray# sizeCapMask 1# s0 of
    (# s1, capMask# #) -> case readMutVar# backing s1 of
      (# s2, backing# #) ->
        case unsafeInsert capMask# backing# k (hash (hashKey k)) v of
            IO f -> case f s2 of
              (# s3, added #) -> case readIntArray# sizeCapMask 0# s3 of
                (# s4, sz# #) -> case (if added then writeIntArray# sizeCapMask 0# (sz# +# 1#) s4 else s4) of
                  s5 ->
                    let threeQuartersCap# = 3# *# ((capMask# +# 1#) `quotInt#` 4#)
                    in if isTrue# (sz# +# 1# >=# threeQuartersCap#)
                        then case resize old of IO g -> g s5
                        else (# s5, () #)

unsafeInsert :: forall key value . Eq key => Int# -> MutableArray# RealWorld (Entry key value) -> key -> Int -> value -> IO Bool
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

lookup :: forall key value m . MonadIO m => HashKey key => HashTable key value -> key -> m (Maybe value)
{-# INLINE lookup #-} -- GHC would inline anyway, but forcing this ensures the maybe is never allocated. Maybe instead return an unboxed sum?
lookup HashTable{..} !k = liftIO $ IO $ \s0 ->
  case readIntArray# sizeCapMask 1# s0 of
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

delete :: forall key value . HashKey key => HashTable key value -> key -> IO (Maybe value)
delete HashTable{..} !k = IO $ \s0 ->
  case readIntArray# sizeCapMask 1# s0 of
    (# s1, capMask# #) ->
      case readIntArray# sizeCapMask 0# s1 of
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
              (# s4, val, True #) -> (# writeIntArray# sizeCapMask 0# (sz# -# 1#) s4, Just val #)
              (# s4, _, False #)  -> (# s4, Nothing #)

iterateWithHash :: HashTable key value -> (key -> Int -> value -> IO ()) -> IO ()
iterateWithHash HashTable{..} f = IO $ \s0 ->
  case readIntArray# sizeCapMask 1# s0 of
    (# s1, capMask# #) -> case readMutVar# backing s1 of
      (# s2, backing# #) ->
        let go n s | isTrue# (n ># capMask#) = (# s, () #)
            go n s = 
              case readArray# backing# n s of
                (# s', Entry k1 h1 v1 #) -> case f k1 h1 v1 of IO f' -> case f' s' of (# s'', () #) -> go (n +# 1#) s''
                (# s', _ #) -> go (n +# 1#) s'
        in go 0# s2

resize :: forall key value . Eq key => HashTable key value -> IO ()
resize old@HashTable{..} = do
  IO $ \s ->
    case readIntArray# sizeCapMask 1# s of
      (# s0, capMask# #) ->
        let reqSz = (capMask# +# 1#) *# 2#
        in case newArray# reqSz Empty s0 of
          (# s1, arr #) -> case iterateWithHash old $ \k h v -> void $ unsafeInsert (reqSz -# 1#) arr k h v of
              IO f -> case f s1 of
                (# s2, () #) -> case writeMutVar# backing arr s2 of
                  s3 -> case writeIntArray# sizeCapMask 1# (reqSz -# 1#) s3 of
                    s4 -> (# s4, () #) 
  
