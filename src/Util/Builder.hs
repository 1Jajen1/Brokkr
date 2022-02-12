module Util.Builder (
  Builder
, word8
, word16Host, word16BE, word16LE
, word32Host, word32BE, word32LE
, word64Host, word64BE, word64LE
, int8
, int16Host, int16BE, int16LE
, int32Host, int32BE, int32LE
, int64Host, int64BE, int64LE
) where

import Data.Word
import Data.Int

import Util.Builder.Internal
import qualified Util.Builder.Prim as Prim

word8 :: Word8 -> Builder
word8 = fixedWrite $ Prim.word8

word16Host :: Word16 -> Builder
word16Host = fixedWrite $ Prim.word16Host

word16BE :: Word16 -> Builder
word16BE = fixedWrite $ Prim.word16BE

word16LE:: Word16 -> Builder
word16LE = fixedWrite $ Prim.word16LE

word32Host :: Word32 -> Builder
word32Host = fixedWrite $ Prim.word32Host

word32BE :: Word32 -> Builder
word32BE = fixedWrite $ Prim.word32BE

word32LE:: Word32 -> Builder
word32LE = fixedWrite $ Prim.word32LE

word64Host :: Word64 -> Builder
word64Host = fixedWrite $ Prim.word64Host

word64BE :: Word64 -> Builder
word64BE = fixedWrite $ Prim.word64BE

word64LE:: Word64 -> Builder
word64LE = fixedWrite $ Prim.word64LE

int8 :: Word8 -> Builder
int8 = fixedWrite $ Prim.word8

int16Host :: Int16 -> Builder
int16Host = fixedWrite $ Prim.int16Host

int16BE :: Int16 -> Builder
int16BE = fixedWrite $ Prim.int16BE

int16LE:: Int16 -> Builder
int16LE = fixedWrite $ Prim.int16LE

int32Host :: Int32 -> Builder
int32Host = fixedWrite $ Prim.int32Host

int32BE :: Int32 -> Builder
int32BE = fixedWrite $ Prim.int32BE

int32LE:: Int32 -> Builder
int32LE = fixedWrite $ Prim.int32LE

int64Host :: Int64 -> Builder
int64Host = fixedWrite $ Prim.int64Host

int64BE :: Int64 -> Builder
int64BE = fixedWrite $ Prim.int64BE

int64LE:: Int64 -> Builder
int64LE = fixedWrite $ Prim.int64LE
