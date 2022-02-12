{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Util.Builder.Prim (
  BuilderState
, BoundedWrite(..)
, FixedWrite(..)
, word8
, word16Host, word16BE, word16LE
, word32Host, word32BE, word32LE
, word64Host, word64BE, word64LE
, int8
, int16Host, int16BE, int16LE
, int32Host, int32BE, int32LE
, int64Host, int64BE, int64LE
) where

import GHC.Word
import GHC.Exts
import Util.ByteOrder
import Data.Int

-- Buffer start and end + bytes already written + RealWorld state so we can do IO
type BuilderState = (# Addr#, Addr#, Int#, State# RealWorld #)

data BoundedWrite a = BoundedWrite !Int (a -> BuilderState -> BuilderState)

data FixedWrite a = FixedWrite !Int (a -> BuilderState -> BuilderState)

contraMapFixed :: (b -> a) -> FixedWrite a -> FixedWrite b
contraMapFixed f (FixedWrite sz g) = FixedWrite sz (g . f)

-- Basic types
word8 :: FixedWrite Word8
word8 = FixedWrite 1 $ \(W8# w) (# c, e, sz, s #) ->
  case writeWord8OffAddr# c 0# w s of
    s' -> (# plusAddr# c 1#, e, sz +# 1#, s' #)

word16Host :: FixedWrite Word16
word16Host = FixedWrite 2 $ \(W16# w) (# c, e, sz, s #) ->
  case writeWord16OffAddr# c 0# w s of
    s' -> (# plusAddr# c 1#, e, sz +# 1#, s' #)

word16BE :: FixedWrite Word16
word16BE = contraMapFixed toBE word16Host

word16LE :: FixedWrite Word16
word16LE = contraMapFixed toLE word16Host

word32Host :: FixedWrite Word32
word32Host = FixedWrite 2 $ \(W32# w) (# c, e, sz, s #) ->
  case writeWord32OffAddr# c 0# w s of
    s' -> (# plusAddr# c 1#, e, sz +# 1#, s' #)

word32BE :: FixedWrite Word32
word32BE = contraMapFixed toBE word32Host

word32LE :: FixedWrite Word32
word32LE = contraMapFixed toLE word32Host

word64Host :: FixedWrite Word64
word64Host = FixedWrite 2 $ \(W64# w) (# c, e, sz, s #) ->
  case writeWord64OffAddr# c 0# w s of
    s' -> (# plusAddr# c 1#, e, sz +# 1#, s' #)

word64BE :: FixedWrite Word64
word64BE = contraMapFixed toBE word64Host

word64LE :: FixedWrite Word64
word64LE = contraMapFixed toLE word64Host

int8 :: FixedWrite Int8
int8 = contraMapFixed fromIntegral word8

int16Host :: FixedWrite Int16
int16Host = contraMapFixed fromIntegral word16Host

int16BE :: FixedWrite Int16
int16BE = contraMapFixed toBE int16Host

int16LE :: FixedWrite Int16
int16LE = contraMapFixed toLE int16Host

int32Host :: FixedWrite Int32
int32Host = contraMapFixed fromIntegral word32Host

int32BE :: FixedWrite Int32
int32BE = contraMapFixed toBE int32Host

int32LE :: FixedWrite Int32
int32LE = contraMapFixed toLE int32Host

int64Host :: FixedWrite Int64
int64Host = contraMapFixed fromIntegral word64Host

int64BE :: FixedWrite Int64
int64BE = contraMapFixed toBE int64Host

int64LE :: FixedWrite Int64
int64LE = contraMapFixed toLE int64Host
