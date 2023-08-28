{-# LANGUAGE CPP, DerivingStrategies, LambdaCase, TemplateHaskellQuotes, MagicHash #-}
module Brokkr.NBT.NBTString.Internal (
  NBTString(..)
, parseNBTString
, withNBTString
, putNBTString
, isValidModifiedUtf8
, fromText
, fromUtf8
, toText
, toUtf8
) where

import Brokkr.NBT.NBTError

import Control.DeepSeq
import Control.Monad

import Data.Char (ord)

import Data.Bits

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS

import Data.String

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import FlatParse.Basic qualified as FP

import Foreign.C.Types (CSize(..), CInt(..))

import GHC.Exts (Addr#, Ptr, Int(..))
import GHC.ForeignPtr
import GHC.Word

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

import Mason.Builder qualified as B

-- | Modified utf-8 bytestring
--
-- Differences to utf-8:
-- \NUL => two bytes overlong
-- no 4 byte sequences, surrogates instead
--
-- https://docs.oracle.com/javase/8/docs/api/java/io/DataInput.html#modified-utf-8
newtype NBTString = NBTString ByteString
  deriving newtype (Eq, NFData)

-- This is strictly faster than Ord ByteString, but is no longer compatible with
-- lazy bytestrings order
instance Ord NBTString where
  compare (NBTString (BS.BS f1 l1)) (NBTString (BS.BS f2 l2)) =
    case compare l1 l2 of
      EQ -> BS.accursedUnutterablePerformIO $ do
        BS.unsafeWithForeignPtr f1 $ \p1 ->
          BS.unsafeWithForeignPtr f2 $ \p2 -> do
            i <- c_memcmp p1 p2 $ fromIntegral l1
            pure $! compare i 0
      x -> x
  {-# INLINE compare #-}

instance IsString NBTString where
  fromString [] = NBTString BS.empty
  fromString xs = NBTString . BS.pack $ concatMap (encodeChar (\x -> [x]) (\x y -> [x,y]) (\x y z -> [x,y,z]) (\x1 y1 z1 x2 y2 z2 -> [x1,y1,z1,x2,y2,z2])) xs

instance TH.Lift NBTString where
  liftTyped (NBTString (BS.BS fptr sz)) =
    [|| let !(lit :: Addr#) = $$(TH.unsafeCodeCoerce . TH.litE . TH.bytesPrimL $ TH.mkBytes fptr 0 (fromIntegral sz))
        in NBTString (BS.BS (ForeignPtr lit FinalPtr) sz) ||]

instance Show NBTString where
  -- TODO Change this to decode to String and use that instead
  --  This is for now mainly for debugging
  show (NBTString bs) = '0' : 'x' : concatMap toHex (BS.unpack bs)
    where
      toHex x = [toHexNibble hi, toHexNibble lo]
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

-- | Parse a NBT-String, which is in modified utf8
--
-- Expects a short in big-endian format and then n bytes of valid
-- modified utf-8.
--
-- Returns a slice of the original input, which
-- means the inputs lifetime is bound to this string.
--
-- Either use 'Data.ByteString.copy' or any of the
-- utf8 conversion methods that always copy to release
-- the original input.
parseNBTString :: FP.ParserT st NBTError NBTString
parseNBTString = withAnyWord16be $ \len -> do
  let !(I# len#) = fromIntegral len
  -- This specific conversion from W16 -> Int cannot be negative, so
  -- the check on FP.take makes no sense
  bs <- FP.takeUnsafe# len#
  unless (isValidModifiedUtf8 bs) . FP.err $ InvalidStringEncoding bs
  pure $ NBTString bs
{-# INLINE parseNBTString #-}

-- | CPS version of 'parseNBTString'. Can help GHC avoid an allocation of the 'NBTString'
--
-- Expects a short in big-endian format and then n bytes of valid
-- modified utf-8.
--
-- Returns a slice of the original input, which
-- means the inputs lifetime is bound to this string.
--
-- Either use 'Data.ByteString.copy' or any of the
-- utf8 conversion methods that always copy to release
-- the original input.
withNBTString :: (NBTString -> FP.ParserT st NBTError a) -> FP.ParserT st NBTError a
{-# INLINE withNBTString #-}
withNBTString f = withAnyWord16be $ \len -> do
  let !(I# len#) = fromIntegral len
  -- This specific conversion from W16 -> Int cannot be negative, so
  -- the check on FP.take makes no sense
  bs <- FP.takeUnsafe# len#
  unless (isValidModifiedUtf8 bs) . FP.err $ InvalidStringEncoding bs
  f (NBTString bs)

withAnyWord16be :: (Word16 -> FP.ParserT st e a) -> FP.ParserT st e a
{-# INLINE withAnyWord16be #-}
#ifdef WORDS_BIGENDIAN
withAnyWord16be = FP.withAnyWord16 pure
#else
-- Going byteSwap16 does wordToWord16 . byteSwap16 . word16ToWord
-- This involves an and# (2^16 - 1) which should be redundant?
-- We can remove that by converting the 'Word' result of byteSwap16#
-- directly, but that gives basically no speed up, so not worth
withAnyWord16be f = FP.withAnyWord16 (f . byteSwap16)
#endif

-- | Encode a NBT-String
--
-- Produces a big endian short as a length prefix and directly
-- writes the bytestring.
putNBTString :: NBTString -> B.Builder
putNBTString (NBTString bs) = B.int16BE (fromIntegral $ BS.length bs) <> B.byteString bs
{-# INLINE putNBTString #-}

-- | Check if a bytestring is valid java modified utf8
isValidModifiedUtf8 :: ByteString -> Bool
isValidModifiedUtf8 (BS.BS fptr len) = BS.accursedUnutterablePerformIO $ BS.unsafeWithForeignPtr fptr $ \ptr -> do
  i <- c_is_valid_modified_utf8 ptr (fromIntegral len)
  pure $ i /= 0
{-# INLINE isValidModifiedUtf8 #-}

-- | Convert a utf-8 'Text' into a modified utf-8 'NBTString'
--
-- Always copies.
fromText :: Text -> NBTString
-- TODO Do something more efficient
fromText = fromString . T.unpack

-- | Convert a utf-8 'ByteString' into a modified utf-8 'NBTString'
--
-- Always copies. (For now. Later on it will only copy if the null byte
-- or other invalid modified utf-8 is present. Some utf-8 is also valid modified utf-8 and vice versa)
fromUtf8 :: ByteString -> NBTString
-- TODO Do something more efficient
fromUtf8 = fromString . T.unpack . T.decodeUtf8

-- | Convert a modified utf-8 'NBTString' into a utf-8 'Text'
--
-- Always copies.
toText :: NBTString -> Text
-- TODO Actual decoding
toText (NBTString bs) = T.decodeUtf8 bs

-- | Convert a modified utf-8 'NBTString' into a utf-8 'ByteString'
--
-- Always copies. (For now. Later on it will only copy if the null byte
-- or other invalid modified utf-8 is present. Some utf-8 is also valid modified utf-8 and vice versa)
toUtf8 :: NBTString -> ByteString
-- TODO
toUtf8 = error "TODO"

encodeChar ::
     (Word8 -> m)
  -> (Word8 -> Word8 -> m)
  -> (Word8 -> Word8 -> Word8 -> m)
  -> (Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> m)
  -> Char -> m
{-# INLINE encodeChar #-}
encodeChar onOne onTwo onThree onPair c
  | cp == 0       = onTwo 0xC0 0x80
  | cp <= 127     = onOne $ fromIntegral cp
  | cp <= 2047    = onTwo
                      (0xC0 .|. fromIntegral (cp `unsafeShiftR` 6))
                      (0x80 .|. fromIntegral (cp .&. 0x3F))
  | cp <= 0xD7FF  = onThree 
                      (0xE0 .|. fromIntegral (cp `unsafeShiftR` 12))
                      (0x80 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x3F))
                      (0x80 .|. fromIntegral (cp .&. 0x3F))
  | cp >= 0xE000 && cp <= 0xFFFF
                  = onThree
                      (0xE0 .|. fromIntegral (cp `unsafeShiftR` 12))
                      (0x80 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x3F))
                      (0x80 .|. fromIntegral (cp .&. 0x3F))
  | cp >= 0x10000 = onPair
                      0xED
                      (0xA0 .|. fromIntegral ((cp `unsafeShiftR` 16) - 1))
                      (0x80 .|. fromIntegral ((cp `unsafeShiftR` 10) .&. 0x3F))
                      0xED
                      (0xB0 .|. fromIntegral ((cp `unsafeShiftR` 6) .&. 0x7))
                      (0x80 .|. fromIntegral (cp .&. 0x3F))
  | otherwise     = error $ "invalid unicode code point " <> show cp
  where cp = ord c

-- TODO Also return if this is valid utf-8. A bytestring is both valid modified-utf-8 and utf-8 if it has no surrogates
-- Maybe rewrite is_valid_modified_utf8 in cmm, to make the c-call basically free
foreign import ccall unsafe "is_valid_modified_utf8" c_is_valid_modified_utf8 :: Ptr Word8 -> CSize -> IO CInt
foreign import ccall unsafe "memcmp" c_memcmp :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt
