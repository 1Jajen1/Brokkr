{-# LANGUAGE MagicHash #-}
module Network.Util.VarNum (
  VarInt(..)
, writeVarNumInternal -- TODO ...
, varIntSize
) where

import Data.Int
import Util.Binary
import FlatParse.Basic
import Data.Word
import Data.Bits
import Control.Monad

import qualified Mason.Builder as B
import qualified Data.ByteString.Builder.Prim.Internal as Prim
import Data.Primitive.Ptr
import GHC.Exts

newtype VarInt = VarInt Int32
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

instance FromBinary VarInt where
  get = VarInt <$> readVarNum (fromIntegral . fromIntegral @Word64 @Word32) 5
  {-# INLINE get #-}

instance ToBinary VarInt where
  put (VarInt i32) = writeVarNum 5 (fromIntegral . fromIntegral @_ @Word32) i32
  {-# INLINE put #-}

-- TODO Optimize this, quite a bit of potential here I think
readVarNum :: (Word64 -> a) -> Int -> Parser e a
readVarNum f maxSz = go 0 0
  where
    go acc res = do
      b <- anyWord8
      let val    = b .&. 0b01111111
          newRes = res .|. (fromIntegral val `shiftL` (7 * acc))
          newAcc = acc + 1
      when (newAcc > maxSz) empty
      case b .&. 0b10000000 of
        0 -> return $ f newRes
        _ -> go newAcc newRes
{-# INLINE readVarNum #-}

writeVarNum :: Int -> (a -> Word64) -> a -> B.Builder
writeVarNum maxSz f = \a -> B.primBounded varNumPrim a
  where
    varNumPrim = Prim.boundedPrim maxSz $ \a ptr -> writeVarNumInternal (f a) ptr
{-# INLINE writeVarNum #-}

-- TODO This can be improved by first checking how many bytes we need (using clz or 0xFFFFFFFF << n * 7 == 0 where n is the number of bytes needed)
-- Then we can do the write at once and thus eliminate the need for loops to just one branch.
-- Probably best to just generate this with TH...
writeVarNumInternal :: Word64 -> Ptr Word8 -> IO (Ptr Word8)
writeVarNumInternal !n !ptr
  | n < 128 = do
      writeOffPtr ptr 0 (fromIntegral n)
      pure $ advancePtr ptr 1
  | otherwise = do
      writeOffPtr ptr 0 . fromIntegral $ setBit (n .&. 127) 7
      writeVarNumInternal (unsafeShiftR n 7) (advancePtr ptr 1)

-- TODO Bench against repeated shift and mask if statements
varIntSize :: Int -> Int
varIntSize x = lookupByteN arr# (countLeadingZeros $ fromIntegral @_ @Int32 x)
  where arr# = "\ENQ\ENQ\ENQ\ENQ\EOT\EOT\EOT\EOT\EOT\EOT\EOT\ETX\ETX\ETX\ETX\ETX\ETX\ETX\STX\STX\STX\STX\STX\STX\STX\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH"#
{-# INLINE varIntSize #-}

lookupByteN :: Addr# -> Int -> Int
lookupByteN addr# (I# n) = I# (word2Int# word#)
  where word# = word8ToWord# (indexWord8OffAddr# addr# n)
{-# INLINE lookupByteN #-}
  
