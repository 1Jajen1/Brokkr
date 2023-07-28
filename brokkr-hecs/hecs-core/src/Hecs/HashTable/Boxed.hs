{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
module Hecs.HashTable.Boxed (
  HashTable
, new
, insert
, lookup
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
data HashTable key value = HashTable {
  size    :: {-# UNPACK #-} !Int
, capMask :: {-# UNPACK #-} !Int
, backing :: !(MutableArray# RealWorld (Entry key value))
}

data Entry key value :: UnliftedType where
  Empty :: Entry key value
  Entry :: !key -> !Int -> !value -> Entry key value

new :: forall key value . Int -> IO (HashTable key value)
new initSz = IO $ \s ->
  case newArray# iSzP2 Empty s of
    (# s', arr #) -> (# s', HashTable 0 (I# (iSzP2 -# 1#)) arr #)
  where
    !(I# iSzP2) = if initSz == 1 then 1 else 1 `unsafeShiftL` (8 * sizeOf (undefined :: Int) - countLeadingZeros (initSz - 1))

insert :: forall key value . HashKey key => HashTable key value -> key -> value -> IO (HashTable key value)
insert old@HashTable{..} k v = IO (go start#) >>= \case
    True -> if size + 1 >= threeQuartersCap
      then resize (HashTable (size + 1) capMask backing)
      else pure (HashTable (size + 1) capMask backing)
    False -> pure old
  where
    threeQuartersCap = 3 * ((capMask + 1) `unsafeShiftR` 2)
    h = hash (hashKey k)
    !(I# start#) = h .&. capMask
    !(I# capMask#) = capMask
    go :: Int# -> State# RealWorld -> (# State# RealWorld, Bool #)
    go n s | isTrue# (n ># capMask#) = go 0# s
    go n s =
      case readArray# backing n s of
        (# s', Entry k1 h1 _ #) -> if h1 == h && k1 == k
          then case writeArray# backing n (Entry k h v) s' of s'' -> (# s'', False #)
          else go (n +# 1#) s'
        (# s', Empty #) -> case writeArray# backing n (Entry k h v) s' of s'' -> (# s'', True #)

unsafeInsert :: forall key value . Eq key => HashTable key value -> key -> Int -> value -> IO ()
unsafeInsert HashTable{..} k h v = IO (go start#)
  where
    !(I# start#) = h .&. capMask
    !(I# capMask#) = capMask
    go :: Int# -> State# RealWorld -> (# State# RealWorld, () #)
    go n s | isTrue# (n ># capMask#) = go 0# s
    go n s =
      case readArray# backing n s of
        (# s', Entry k1 h1 _ #) -> if h1 == h && k1 == k
          then case writeArray# backing n (Entry k h v) s' of s'' -> (# s'', () #)
          else go (n +# 1#) s'
        (# s', Empty #) -> case writeArray# backing n (Entry k h v) s' of s'' -> (# s'', () #)

lookup :: forall key value r m . MonadIO m => HashKey key => HashTable key value -> key -> (value -> m r) -> m r -> m r
lookup HashTable{..} k cont notFound = liftIO (IO (go start#)) >>= \case
  Just v -> cont v
  Nothing -> notFound
  where
    h = hash (hashKey k)
    !(I# start#) = h .&. capMask
    !(I# capMask#) = capMask
    go :: Int# -> State# RealWorld -> (# State# RealWorld, Maybe value #)
    go n s | isTrue# (n ># capMask#) = go 0# s
    go n s =
      case readArray# backing n s of
        (# s', Entry k1 h1 v1 #) -> if h1 == h && k1 == k then (# s', Just v1 #) else go (n +# 1#) s'
        (# s', Empty #) -> (# s', Nothing #)
{-# INLINE lookup #-}

iterateWithHash :: HashTable key value -> (key -> Int -> value -> IO ()) -> IO ()
iterateWithHash HashTable{..} f = IO (go 0#)
  where
    !(I# capMask#) = capMask
    go n s | isTrue# (n ># capMask#) = (# s, () #)
    go n s = 
      case readArray# backing n s of
        (# s', Entry k1 h1 v1 #) -> case f k1 h1 v1 of IO f' -> case f' s' of (# s'', () #) -> go (n +# 1#) s''
        (# s', Empty #) -> go (n +# 1#) s'

resize :: forall key value . Eq key => HashTable key value -> IO (HashTable key value)
resize old@HashTable{..} = do
  newHT <- IO $ \s ->
    case newArray# reqSz Empty s of
      (# s', arr #) -> (# s', HashTable size (I# (reqSz -# 1#)) arr #)
  iterateWithHash old $ \k h v -> unsafeInsert newHT k h v
  pure newHT
  where
    !(I# capMask#) = capMask
    reqSz = (capMask# +# 1#) *# 2#
