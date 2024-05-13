{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- {-# OPTIONS_GHC -dsuppress-all -ddump-simpl #-}
module Brokkr.NBT.Validate (
  skipNBT
, skipCompound
, skipTag
) where

import Data.Word

import FlatParse.Basic qualified as FP

import GHC.Exts

import Brokkr.NBT.NBTError
import Brokkr.NBT.NBTString

-- TODO Use this in codec

skipNBT :: FP.ParserT st NBTError ()
{-# INLINE skipNBT #-}
skipNBT = do
  t <- FP.anyWord8
  if t == 0 then pure () else skipKey $ skipTag t

skipCompound :: FP.ParserT st NBTError ()
{-# INLINE skipCompound #-}
skipCompound = do
  t <- FP.anyWord8
  if t == 0 then pure () else skipKey $ skipTag t >> skipCompound

-- Yes, taking the extra parser as a continuation does improve both code gen
--  and the performance
skipKey :: FP.ParserT st NBTError () -> FP.ParserT st NBTError ()
{-# INLINE skipKey #-}
skipKey f = withNBTString $ \_ -> f

-- TODO I need to perform a heap check here to avoid hogging a thread.
-- This is 100% allocation free and thus very dangerous when applied to
-- user input, especially if that input came compressed.
skipTag :: Word8 -> FP.ParserT st NBTError ()
{-# INLINE skipTag #-}
-- int8, int16, int32, int64, float, double
skipTag 1 = FP.skip 1
skipTag 2 = FP.skip 2
skipTag 3 = FP.skip 4
skipTag 4 = FP.skip 8
skipTag 5 = FP.skip 4
skipTag 6 = FP.skip 8
-- byte/int/long array
skipTag 7  = FP.anyWord32be >>= skipUnsafe . fromIntegral
skipTag 11 = FP.anyWord32be >>= \sz -> skipUnsafe $ fromIntegral sz * 4
skipTag 12 = FP.anyWord32be >>= \sz -> skipUnsafe $ fromIntegral sz * 8
-- strings
skipTag 8 = skipKey (pure ())
-- lists
skipTag 9 = do
  t <- FP.anyWord8
  sz <- FP.anyInt32be
  let go  0 = pure ()
      go !n = skipTag t >> go (n - 1)
  go sz
-- compounds
skipTag 10 = skipCompound
-- anything else is bad
skipTag _ = FP.empty

-- Copy of skip that assumes non-negative size
-- This makes sense for the cases above because they are constructed
-- from 32 bit unsigned integers. This is also safe because the parser
-- will just fail anyway if it was a negative 32 bit number
skipUnsafe :: Int -> FP.ParserT st e ()
{-# INLINE skipUnsafe #-}
skipUnsafe (I# i) = FP.withEnsure# i $ FP.ParserT $ \_ _ s st ->
  FP.OK# st () (plusAddr# s i)
