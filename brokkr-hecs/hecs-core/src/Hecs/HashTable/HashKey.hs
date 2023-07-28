module Hecs.HashTable.HashKey (
  HashKey(..)
, HashFn(..)
, hashWithSalt
, hash
) where
import Data.Bits

-- Just testing this for fun, no clue if this is good
newtype HashFn = HashFn (Int -> Int)

hash :: HashFn -> Int
hash = hashWithSalt 0
{-# INLINE hash #-}

hashWithSalt :: Int -> HashFn -> Int
hashWithSalt s (HashFn fn) = fn s
{-# INLINE hashWithSalt #-}

instance Semigroup HashFn where
  HashFn f <> HashFn g = HashFn $ g . f
  {-# INLINE (<>) #-}

instance Monoid HashFn where
  mempty = HashFn id
  {-# INLINE mempty #-}

class Eq key => HashKey key where
  hashKey :: key -> HashFn

instance HashKey Int where
  hashKey x = HashFn $ \r -> r `xor` fromIntegral wfin
    where
      m1 = 0xff51afd7ed558ccd
      m2 = 0xc4ceb9fe1a85ec53
      w0 :: Word = fromIntegral x
      w1 = w0 `xor` unsafeShiftR w0 33
      w2 = m1 * w1
      w3 = w2 `xor` unsafeShiftR w2 33
      w4 = w3 * m2
      wfin = w4 `xor` unsafeShiftR w4 33
  {-# INLINE hashKey #-}
