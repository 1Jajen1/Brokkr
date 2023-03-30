{-# LANGUAGE DerivingStrategies, LambdaCase #-}
module Brokkr.NBT.NBTString.Internal (
  NBTString(..)
, parseNBTString
, putNBTString
, isValidModifiedUtf8
) where

import Control.Monad

import Data.Bits

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS

import Data.Word (Word8)

import FlatParse.Basic qualified as FP

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CSize(..), CInt(..))

import Mason.Builder qualified as B

-- Modified utf-8 bytestring
-- Differences to utf-8:
-- \NUL => two bytes overlong
-- no 4 byte sequences, surrogates instead
newtype NBTString = NBTString ByteString
  -- TODO When I have lookup benchmarks:
  -- ByteString checks bytes first, length after. I think specifically for
  -- lookups length first might be faster as we don't load the string
  deriving newtype (Eq, Ord)

instance Show NBTString where
  show (NBTString bs) = '0' : 'x' : concatMap toHex (BS.unpack bs)
    where
      toHex x = toHexNibble hi : toHexNibble lo : []
        where
          hi = x `unsafeShiftR` 4
          lo = x .&. 7
          toHexNibble = \case
            0  -> '0'
            1  -> '1'
            2  -> '2'
            3  -> '3'
            4  -> '4'
            5  -> '5'
            6  -> '6'
            7  -> '7'
            8  -> '8'
            9  -> '9'
            10 -> 'A'
            11 -> 'B'
            12 -> 'C'
            13 -> 'D'
            14 -> 'E'
            15 -> 'F'
            _  -> error "Unreachable. 4 bits are in range 0..15"

parseNBTString :: FP.ParserT st e NBTString
parseNBTString = do
  len <- FP.anyWord16be
  bs  <- FP.take $ fromIntegral len
  when (not $ isValidModifiedUtf8 bs) FP.empty
  pure $ NBTString bs
{-# INLINE parseNBTString #-}

putNBTString :: NBTString -> B.Builder
putNBTString (NBTString bs) = B.int16BE (fromIntegral $ BS.length bs) <> B.byteString bs
{-# INLINE putNBTString #-}

isValidModifiedUtf8 :: ByteString -> Bool
isValidModifiedUtf8 (BS.BS _ 0) = True
isValidModifiedUtf8 (BS.BS fptr len) = BS.accursedUnutterablePerformIO $ BS.unsafeWithForeignPtr fptr $ \ptr -> do
  i <- c_is_valid_modified_utf8 ptr (fromIntegral len)
  pure $ i /= 0
{-# INLINE isValidModifiedUtf8 #-}

-- TODO Also return if this is valid utf-8. A bytestring is both valid modified-utf-8 and utf-8 if it has no surrogates
-- Maybe rewrite is_valid_modified_utf8 in cmm, to make the c-call basically free
foreign import ccall unsafe "is_valid_modified_utf8" c_is_valid_modified_utf8 :: Ptr Word8 -> CSize -> IO CInt
