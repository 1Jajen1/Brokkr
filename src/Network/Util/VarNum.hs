{-# OPTIONS_GHC -O2 -ddump-rule-firings -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-module-prefixes -dsuppress-type-signatures -dsuppress-uniques #-}
{-# LANGUAGE UnboxedTuples #-}
module Network.Util.VarNum (
  VarInt(..)
, test
) where

import Data.Int
import Util.Binary
import FlatParse.Basic
import Data.Word
import Data.Bits
import Control.Monad

import qualified Util.Builder.Internal as B
import qualified Util.Builder.Prim as Prim
import qualified Util.Builder as B

newtype VarInt = VarInt Int32

instance FromBinary VarInt where
  get = VarInt <$> readVarNum (fromIntegral . fromIntegral @Word64 @Word32) 5
  {-# INLINE get #-}

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
writeVarNum maxSz f = B.boundedWrite . Prim.BoundedWrite maxSz $ \a st -> go (f a) st
  where
    runFixed (Prim.FixedWrite _ g) = g
    -- TODO Check why this is not joinrec
    go :: Word64 -> Prim.BuilderState -> Prim.BuilderState
    go 0 st = runFixed Prim.word8 0 st
    go n st =
      let temp :: Word8 = fromIntegral n .&. 0b01111111
          newValue = unsafeShiftR n 7
          toWrite = if newValue == 0
            then temp
            else temp .|. 0b10000000
          st' = runFixed Prim.word8 toWrite st
      in if newValue == 0 then st' else go newValue st'
{-# INLINE writeVarNum #-}
{-
writeVarNum f a = go (f a)
  where
    go :: Word64 -> B.Builder
    go 0 = B.word8 0
    go n =
      let temp :: Word8 = fromIntegral n .&. 0b01111111
          newValue = shiftR n 7
          toWrite = if newValue == 0
            then temp
            else temp .|. 0b10000000
      in B.word8 toWrite <> if newValue == 0 then mempty else go newValue
-}

test = writeVarNum 5 id 10 <> B.word8 10 <> B.word8 10

varIntSize :: Int -> Int
varIntSize nr | nr < 0 = 5
varIntSize nr | nr >= 0 && nr < shiftL 2 7 = 1
varIntSize nr | nr >= shiftL 2 7 && nr < shiftL 2 14 = 2
varIntSize nr | nr >= shiftL 2 14 && nr < shiftL 2 14 = 3
varIntSize nr | nr >= shiftL 2 21 && nr < shiftL 2 28 = 4

