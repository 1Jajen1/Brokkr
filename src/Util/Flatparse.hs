{-# LANGUAGE MagicHash #-}
module Util.Flatparse (
  anyWord16
, anyWord32
, anyWord64
, takeN
, takeRemaining
) where

import FlatParse.Basic hiding (anyWord16, anyWord32)
import Data.Word
import Data.ByteString
import qualified Data.ByteString.Internal as B
import GHC.Exts
import GHC.Word
import GHC.ForeignPtr

anyWord16 :: Parser e Word16
anyWord16 = Parser $ \_ eob buf -> case 2# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexWord16OffAddr# buf 0# of
    w -> OK# (W16# w) (plusAddr# buf 2#)
{-# INLINE anyWord16 #-}

anyWord32 :: Parser e Word32
anyWord32 = Parser $ \_ eob buf -> case 4# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexWord32OffAddr# buf 0# of
    w -> OK# (W32# w) (plusAddr# buf 4#)
{-# INLINE anyWord32 #-}

anyWord64 :: Parser e Word64
anyWord64 = Parser $ \_ eob buf -> case 8# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexWord64OffAddr# buf 0# of
    w -> OK# (W64# w) (plusAddr# buf 8#)
{-# INLINE anyWord64 #-}

-- | Read n bytes from the input.
--
-- The 'Bytestring' this function creates will keep the entire input in memory
-- until it itself is released. 
takeN :: Int -> Parser e ByteString
takeN 0 = pure mempty
takeN nn@(I# n) = Parser $ \fp eob s ->
  let len = minusAddr# eob s
  in case len >=# n of
    1# -> OK# (B.BS (ForeignPtr s fp) nn) (plusAddr# s n)
    _ -> Fail#
{-# INLINE takeN #-}

-- | Read the remaining bytes from the input.
--
-- The 'Bytestring' this function creates will keep the entire input in memory
-- until it itself is released. 
takeRemaining :: Parser e ByteString
takeRemaining = Parser $ \fp eob s -> OK# (B.BS (ForeignPtr s fp) (I# (minusAddr# eob s))) eob
{-# INLINE takeRemaining #-}
