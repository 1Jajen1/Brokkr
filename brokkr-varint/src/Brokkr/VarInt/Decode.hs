{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnboxedTuples #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Brokkr.VarInt.Decode (
  withVarInt, withVarInt#
, VarIntDecodeError(..)
-- TODO Internal module
, withVarIntLoop, withVarIntLoop#
, pextDecode, pextDecode# 
) where

import Data.Proxy

import FlatParse.Basic qualified as Flatparse

import GHC.Exts
import GHC.TypeLits

-- | Collection of errors which may occur when parsing variable-length numbers
--
-- Currently only contains 'OverMaxSize', which refers to minecrafts size restriction
-- on 'VarInt' and 'VarLong' (5 and 10 bytes)
newtype VarIntDecodeError = OverMaxSize Int
  deriving stock Show

-- | CPS style parser for variable-length encoded numbers
--
-- Needs a statically known maximum size and specializes for 5 bytes.
-- 
-- Also requires a handler for errors.
--
-- It is not always trivial to have GHC unbox even with the callback,
-- so a manually unboxed version is available: 'withVarInt#'
withVarInt :: forall maxSize a e st . KnownNat maxSize
  => (VarIntDecodeError -> e) -> (Int -> Flatparse.ParserT st e a) -> Flatparse.ParserT st e a
{-# INLINE withVarInt #-}
withVarInt = case natVal (Proxy @maxSize) of
  5 -> pextDecode
  -- TODO Specialize the VarLong variant
  _ -> withVarIntLoop @maxSize

-- | Manually unboxed version of 'withVarInt'.
withVarInt# :: forall maxSize (st :: ZeroBitType) . KnownNat maxSize
  => Addr# -> Addr# -> st -> (# st, (# (# Int#, Addr# #) | (# #) | (# VarIntDecodeError #) #) #)
{-# INLINE withVarInt# #-}
withVarInt# = case natVal (Proxy @maxSize) of
  5 -> pextDecode#
  -- TODO Specialize the VarLong variant
  _ -> withVarIntLoop# @maxSize

-- | Loop version of variable-length decoding.
--
-- Used if not enough bytes are present for the branchless parse
--
-- A manually unboxed version exists: 'withVarIntLoop#'
withVarIntLoop :: forall maxSize a e st . KnownNat maxSize
  => (VarIntDecodeError -> e) -> (Int -> Flatparse.ParserT st e a) -> Flatparse.ParserT st e a
{-# INLINE withVarIntLoop #-}
withVarIntLoop err f = Flatparse.ParserT $ \fp eob s st -> case withVarIntLoop# @maxSize eob s st of
  (# st', (# | | (# e #) #) #) -> Flatparse.Err# st' (err e)
  (# st', (# | (# #) | #) #) -> Flatparse.Fail# st'
  (# st', (# (# i, s' #) | | #) #) -> case f (I# i) of
    Flatparse.ParserT g -> g fp eob s' st' 

-- Manually unboxed version of 'withVarIntLoop'
withVarIntLoop# :: forall maxSize (st :: ZeroBitType) . KnownNat maxSize
  => Addr# -> Addr# -> st -> (# st, (# (# Int#, Addr# #) | (# #) | (# VarIntDecodeError #) #) #)
{-# INLINE withVarIntLoop# #-}
withVarIntLoop# eob s0 st = go 0# 0## s0
  where
    !(I# maxSz) = 7 * fromIntegral (natVal (Proxy @maxSize))
    go acc res s
      | isTrue# (neAddr# eob s) =
        let b = word8ToWord# (indexWord8OffAddr# s 0#)
            val = b `and#` 0b01111111##
            newRes = res `or#` (val `uncheckedShiftL#` acc)
            newAcc = acc +# 7#
        in case b `and#` 0b10000000## of
          0## -> (# st, (# (# word2Int# newRes, plusAddr# s 1# #) | | #) #)
          _   -> if isTrue# (newAcc >=# maxSz)
            then (# st, (# | | (# OverMaxSize (I# (newAcc `quotInt#` 7#)) #) #) #)
            else go newAcc newRes (plusAddr# s 1#)
      | otherwise = (# st, (# | (# #) | #) #) 

-- | Branchless version of variable-length decoding.
--
-- Outperforms the loop based version on architectures supporting fast
-- pext instructions (most modern x86), but requires 8 bytes to be available.
--
-- A manually unboxed version is available: 'pextDecode#'
pextDecode :: (VarIntDecodeError -> e) -> (Int -> Flatparse.ParserT st e a) -> Flatparse.ParserT st e a
{-# INLINE pextDecode #-}
pextDecode err f = Flatparse.ParserT $ \fp eob s st -> case pextDecode# eob s st of
  (# st', (# | | (# e #) #) #) -> Flatparse.Err# st' (err e)
  (# st', (# | (# #) | #) #) -> Flatparse.Fail# st'
  (# st', (# (# i, s' #) | | #) #) -> case f (I# i) of
    Flatparse.ParserT g -> g fp eob s' st' 

-- | Manually unboxed version of 'pextDecode'
pextDecode# :: forall (st :: ZeroBitType) .
  Addr# -> Addr# -> st -> (# st, (# (# Int#, Addr# #) | (# #) | (# VarIntDecodeError #) #) #)
pextDecode# eob s st =
  if isTrue# (8# ># minusAddr# eob s)
    -- Just go to the loop if this is small...
    -- It is possible to use the other code here as well, but that
    -- requires some way to read n bytes of an addr into a register
    -- and haskell -> cmm cannot do that nicely.
    -- It would need to be asm or c which has ffi overhead
    -- This really only hurts VarInts at the end of input...
    then withVarIntLoop# @5 eob s st
    else
      let w = indexWordOffAddr# s 0#
          msbs = not# w `and#` not# 0x7f7f7f7f7f7f7f7f##
          len = plusWord# 1## (ctz# msbs)
          varintPart = w `and#` (msbs `xor#` minusWord# msbs 1##)
          res = fromS1 varintPart
          bytesConsumed = len `uncheckedShiftRL#` 3#
      in if isTrue# (5## `ltWord#` bytesConsumed)
        then (# st, (# | | (# OverMaxSize (I# (word2Int# bytesConsumed)) #) #) #)
        else (# st, (# (# word2Int# res, plusAddr# s (word2Int# bytesConsumed) #) | | #) #)

fromS1 :: Word# -> Word#
{-# INLINE fromS1 #-}
-- pext is fine? Why the fuck is pdep not?
fromS1 w = pext# w 0x0000000f7f7f7f7f##
-- fromS1 w =
--         ( w `and#` 0x000000000000007f##)
--   `or#` ((w `and#` 0x0000000f00000000##) `uncheckedShiftRL#` 4#)
--   `or#` ((w `and#` 0x000000007f000000##) `uncheckedShiftRL#` 3#)
--   `or#` ((w `and#` 0x00000000007f0000##) `uncheckedShiftRL#` 2#)
--   `or#` ((w `and#` 0x0000000000007f00##) `uncheckedShiftRL#` 1#)
