module Util.ByteOrder (
  toBE, toLE
) where

import Data.Int
import Data.Word
import GHC.ByteOrder
import Unsafe.Coerce

-- TODO Names ...
toBE :: ByteSwap a => a -> a
toBE = case targetByteOrder of
  BigEndian -> id
  LittleEndian -> swapBytes
{-# INLINE toBE #-}

toLE :: ByteSwap a => a -> a
toLE = case targetByteOrder of
  BigEndian -> swapBytes
  LittleEndian -> swapBytes
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

instance ByteSwap Word32 where
  swapBytes = byteSwap32
  {-# INLINE swapBytes #-}

instance ByteSwap Word64 where
  swapBytes = byteSwap64
  {-# INLINE swapBytes #-}
