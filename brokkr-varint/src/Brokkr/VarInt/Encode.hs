{-# LANGUAGE AllowAmbiguousTypes #-}
module Brokkr.VarInt.Encode (
  unsafePutVarInt
-- TODO Move to some internal module?
, writeVarNumInternal
, pdepEncodeVarInt
) where

import Data.ByteString.Builder.Prim.Internal qualified as Prim

import Data.Bits
import Data.Coerce
import Data.Primitive.Ptr
import Data.Proxy
import Data.Word

import GHC.TypeLits

import Mason.Builder qualified as Mason

-- | Encode any 'Word' with a given max size into a variable-length number
--
-- Named unsafe because no actual checks are made that the number fits into
-- the maximum amount of bytes. The size is merely used to specialize
-- for better performance on the 5 byte case (ie 'VarInt').
--
-- The two lower level functions are 'writeVarNumInternal' and 'pdepEncodeVarInt'
unsafePutVarInt :: forall maxSize . KnownNat maxSize => Word -> Mason.Builder
{-# INLINE unsafePutVarInt #-}
unsafePutVarInt i = Mason.primBounded varIntPrim i
  where
    maxSz = fromIntegral $ natVal (Proxy @maxSize)
    varIntPrim = case maxSz of
      5 -> Prim.boundedPrim 8 pdepEncodeVarInt
      -- TODO Write a specialized version for VarLong as well...
      _ -> Prim.boundedPrim maxSz writeVarNumInternal

-- | Encode and write a number to a buffer using variable length encoding
--
-- This method uses a simple loop to write a byte at a time. For
-- 'VarInt' a faster method is 'pdepEncodeVarInt' which performs
-- just one write and does not branch.
writeVarNumInternal :: Word -> Ptr Word8 -> IO (Ptr Word8)
writeVarNumInternal !n !ptr
  | n < 128 = do
      writeOffPtr ptr 0 (fromIntegral n)
      pure $ advancePtr ptr 1
  | otherwise = do
      writeOffPtr ptr 0 . fromIntegral $ setBit (n .&. 127) 7
      writeVarNumInternal (unsafeShiftR n 7) (advancePtr ptr 1)

-- | Encode and write a number to a buffer using variable length encoding
--
-- While often faster than 'writeVarNumInternal', it writes 8 bytes at once.
-- This is not usually a problem, as builders often write left to right in order.
-- However if you must not overwrite the bytes 3-7 bytes after the 'VarInt',
-- consider using the slightly slower 'writeVarNumInternal' instead.
pdepEncodeVarInt :: Word -> Ptr Word8 -> IO (Ptr Word8)
pdepEncodeVarInt !value !ptr = do
  let s1 = toS1 value
      leading = countLeadingZeros s1
      unusedBs = (leading - 1) `unsafeShiftR` 3
      bytesNeeded = 8 - unusedBs
      msbMask = 0xFFFFFFFFFFFFFFFF `unsafeShiftR` ((8 - bytesNeeded + 1) * 8 - 1)
      merged = s1 .|. (0x8080808080808080 .&. msbMask)
  writeOffPtr (coerce ptr) 0 merged
  pure (advancePtr ptr bytesNeeded)

toS1 :: Word -> Word
toS1 !w =
        (w .&. 0x000000000000007f)
   .|. ((w .&. 0x0000000000003f80) `unsafeShiftL` 1)
   .|. ((w .&. 0x00000000001fc000) `unsafeShiftL` 2)
   .|. ((w .&. 0x000000000fe00000) `unsafeShiftL` 3)
   .|. ((w .&. 0x00000000f0000000) `unsafeShiftL` 4)
-- TODO Branch if we have a fast pdep. Sadly I am using zen4 and using pdep here tanks performance
--  which is weird cause I think zen4 should have decent pdep/pext performance
-- toS1 (W# value#) = W# (pdep# value# 0x0000000f7f7f7f7f##)
