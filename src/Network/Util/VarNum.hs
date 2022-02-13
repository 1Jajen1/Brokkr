{-# LANGUAGE UnboxedTuples #-}
module Network.Util.VarNum (
  VarInt(..)
, writeVarNumInternal -- TODO ...
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

newtype VarInt = VarInt Int32

instance FromBinary VarInt where
  get = VarInt <$> readVarNum (fromIntegral . fromIntegral @Word64 @Word32) 5
  {-# INLINE get #-}

instance ToBinary VarInt where
  put (VarInt i32) = writeVarNum 5 fromIntegral i32
  {-# INLINE put #-}

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

-- This function can do bad things if the word requires more than maxSz bytes to be written, so take care not to exceed that!
-- TODO Statically check that somehow?
  -- Actually this is quite safe since this is internal and only used via VarInt/VarLong which both are fixed size and below the maxSz
-- TODO Try to further optimise this
writeVarNum :: Int -> (a -> Word64) -> a -> B.Builder
writeVarNum maxSz f = \a -> B.primBounded varNumPrim a
  where
    varNumPrim = Prim.boundedPrim maxSz $ \a ptr -> writeVarNumInternal (f a) ptr
{-# INLINE writeVarNum #-}

-- TODO
writeVarNumInternal :: Word64 -> Ptr Word8 -> IO (Ptr Word8)
writeVarNumInternal !n !ptr
  | n < unsafeShiftL 2 7 = do
      writeOffPtr ptr 0 (fromIntegral n)
      pure $ advancePtr ptr 1
  | otherwise = do
      writeOffPtr ptr 0 . fromIntegral $ setBit (n .&. 127) 7
      writeVarNumInternal (unsafeShiftR n 7) (advancePtr ptr 1)