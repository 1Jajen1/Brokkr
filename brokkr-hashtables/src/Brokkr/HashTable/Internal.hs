{-# LANGUAGE TypeFamilies #-}
module Brokkr.HashTable.Internal (
  Storage(..)
, HashTable'
, HashTable(..)
, Salt, MaxLoadFactor
, HashFn(..), Hash(..)
) where

import Control.Monad.Primitive

-- | Salt to be used when hashing keys
type Salt = Int
-- | Maximum load factor. The table grows if it exceeds
-- this value.
type MaxLoadFactor = Float

-- TODO Change Boxed to Lifted and add Unlifted

-- | All supported storage backends for keys and values
--
-- * Boxed refers to ordinary lifted haskell heap objects.
-- * Storable refers to types that implement 'Foreign.Storable.Storable'
-- * Prim refers to types that implement 'Data.Primitive.Prim'
data Storage = Boxed | Prim | Storable

-- | Data family unifying all HashTable implementations
data family HashTable' (keyStorage :: Storage) (valueStorage :: Storage) s key value

-- | Typeclass encoding the behavior of a HashTable.
--
-- Necessary because the tables provided by this library are parameterized over how
-- keys and values can be stored.
--
-- See the documentation for more information.
class HashTable keyStorage valueStorage key value where
  -- | Create a new HashTable with the given salt and max load factor setting
  --
  -- The initial table will have some default initial size. Use 'reserve' to grow the
  -- table to a larger size before doing bulk inserts.
  new :: PrimMonad m => Int -> Salt -> MaxLoadFactor -> m (HashTable' keyStorage valueStorage (PrimState m) key value)
  -- | Retrieve the number of key value pairs currently in the HashTable
  size :: PrimMonad m => HashTable' keyStorage valueStorage (PrimState m) key value -> m Int
  -- | Reserve space for n elements
  --
  -- Does not reserve an additional amount of space if the table can already hold n elements.
  -- Get and add the size to the number of elements to reserve if that is the intention.
  --
  -- 'insert' may require the table to grow. Using 'reserve' prior to
  -- bulk insertion can significantly lower the number of resizes.
  --
  -- Reserves space for @nextPowerOf2 (n / maxLoadFactor)@ elements
  reserve :: (PrimMonad m, Eq key, Hash key) => HashTable' keyStorage valueStorage (PrimState m) key value -> Int -> m ()
  -- | Lookup the value associated with the key
  --
  -- See 'lookupWithHash' if you already have a precalculated hash.
  lookup :: (PrimMonad m, Eq key, Hash key) => HashTable' keyStorage valueStorage (PrimState m) key value -> key -> (value -> m r) -> m r -> m r 
  -- | Lookup the value associated with the key given a precalculated hash
  lookupWithHash :: (PrimMonad m, Eq key) => HashTable' keyStorage valueStorage (PrimState m) key value -> key -> Int -> (value -> m r) -> m r -> m r
  -- | Insert a key value pair
  --
  -- May grow the table, which is a very expensive operation. If multiple keys
  -- are inserted in a row, consider using 'reserve' first!
  --
  -- See 'insertWithHash' if you already have a precalculated hash.
  insert :: (PrimMonad m, Eq key, Hash key) => HashTable' keyStorage valueStorage (PrimState m) key value -> key -> value -> m ()
  -- | Insert a key value pair given a precalculated hash
  --
  -- May grow the table, which is a very expensive operation. If multiple keys
  -- are inserted in a row, consider using 'reserve' first!
  insertWithHash :: (PrimMonad m, Eq key, Hash key) => HashTable' keyStorage valueStorage (PrimState m) key value -> key -> Int -> value -> m ()
  -- | Remove a key value pair. Returns the removed value if it existed. 
  --
  -- See 'deleteWithHash' if you already have a precalculated hash.
  delete :: (PrimMonad m, Eq key, Hash key) => HashTable' keyStorage valueStorage (PrimState m) key value -> key -> m (Maybe value)
  -- | Remove a key value pair given a precalculated hash. Returns the removed value if it existed. 
  deleteWithHash :: (PrimMonad m, Eq key) => HashTable' keyStorage valueStorage (PrimState m) key value -> key -> Int -> m (Maybe value)
  -- | Monadically (left)-fold over all values of the table. The accumulator is strict.
  foldM :: PrimMonad m => HashTable' keyStorage valueStorage (PrimState m) key value -> (z -> key -> value -> m z) -> m z -> m z

-- | Hash function
--
-- The input integer is the current state of the hash process
-- and should always be used to derive the result, unless you
-- know what you are doing. Otherwise the composition of multiple
-- 'HashFn's makes no sense.
newtype HashFn = HashFn (Int -> Int)

instance Semigroup HashFn where
  HashFn f <> HashFn g = HashFn (g . f)
  {-# INLINE (<>) #-}

instance Monoid HashFn where
  mempty = HashFn id
  {-# INLINE mempty #-}

-- | Bring your own hash function. This is purposely not implemented, no default is given,
-- but it is statically required to pass a salt on creation and have an instance of
-- Hash in scope for most methods.
--
-- The salt may or may not be used: instance Hash Int where hash x = HashFn (const x)
-- for example. This is obviously the fastest "hash", but may not have the best hashtable
-- behavior (this depends on your keys!) so benchmark
class Hash a where
  hash :: a -> HashFn
