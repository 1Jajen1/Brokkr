module Util.ByteOrder (
  toBE, toLE
) where

import Data.Int
import Data.Word
import GHC.ByteOrder
import Unsafe.Coerce
import Util.Word24

-- | Swap byteorder if we are on a little endian system.
--
-- Minecraft Java Edition uses BigEndian everywhere thus we need this whenever parsing numbers
toBE :: ByteSwap a => a -> a
toBE = case targetByteOrder of
  BigEndian -> id
  LittleEndian -> swapBytes
{-# INLINE toBE #-}

-- | Swap byteorder if we are on a big endian system.
--
-- Mainly included for parity, not sure if I even need this.
toLE :: ByteSwap a => a -> a
toLE = case targetByteOrder of
  BigEndian -> swapBytes
  LittleEndian -> id
{-# INLINE toLE #-}

class ByteSwap a where
  swapBytes :: a -> a

instance ByteSwap Int16 where
  swapBytes = unsafeCoerce . byteSwap16 . unsafeCoerce
  {-# INLINE swapBytes #-}

instance ByteSwap Int32 where
  swapBytes = unsafeCoerce . byteSwap32 . unsafeCoerce
  {-# INLINE swapBytes #-}

instance ByteSwap Int64 where
  swapBytes = unsafeCoerce . byteSwap64 . unsafeCoerce
  {-# INLINE swapBytes #-}

instance ByteSwap Word16 where
  swapBytes = byteSwap16
  {-# INLINE swapBytes #-}

instance ByteSwap Word24 where
  swapBytes = byteSwap24
  {-# INLINE swapBytes #-}

instance ByteSwap Word32 where
  swapBytes = byteSwap32
  {-# INLINE swapBytes #-}

instance ByteSwap Word64 where
  swapBytes = byteSwap64
  {-# INLINE swapBytes #-}
