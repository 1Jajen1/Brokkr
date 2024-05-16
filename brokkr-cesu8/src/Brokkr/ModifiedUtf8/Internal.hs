{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module Brokkr.ModifiedUtf8.Internal (
  ModifiedUtf8(..)
, pack, unpack
, mkModifiedUtf8, unsafeMkModifiedUtf8
, isValidModifiedUtf8
) where

import Control.DeepSeq

import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BS
import Data.String
import Data.Word

import Foreign.C.Types

import GHC.Exts
import GHC.ForeignPtr

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

import Brokkr.Common (Variant(..))
import Brokkr.Common qualified as Common

-- | ByteString in valid modified utf8
--
-- See https://docs.oracle.com/javase/8/docs/api/java/io/DataInput.html
newtype ModifiedUtf8 = ModifiedUtf8 { unwrapModifiedUtf8 :: ByteString }
  deriving stock Show
  deriving newtype (Eq, Ord, Semigroup, Monoid, NFData)

instance TH.Lift ModifiedUtf8 where
  liftTyped (ModifiedUtf8 (BS.BS fptr sz)) =
    [|| let !(lit :: Addr#) = $$(TH.unsafeCodeCoerce . TH.litE . TH.bytesPrimL $ TH.mkBytes fptr 0 (fromIntegral sz))
        in ModifiedUtf8 (BS.BS (ForeignPtr lit FinalPtr) sz) ||]

instance IsString ModifiedUtf8 where
  fromString = pack
  {-# INLINE fromString #-}

-- | Create a modified utf8 string from a haskell string
--
-- Surrogate characters are replaced
pack :: String -> ModifiedUtf8
{-# INLINE pack #-}
pack = ModifiedUtf8 . Common.pack Java

-- | Create a haskell String from a valid modified utf8 string
--
-- Acts as a good producer for list fusion
unpack :: ModifiedUtf8 -> String
{-# INLINE unpack #-}
unpack = Common.unpack . coerce

-- | Create a modified utf8 string from a 'ByteString'
--
-- Validates the string first and returns 'Nothing' on failure.
mkModifiedUtf8 :: ByteString -> Maybe ModifiedUtf8
{-# INLINE mkModifiedUtf8 #-}
mkModifiedUtf8 bs
  | isValidModifiedUtf8 bs = Just $ ModifiedUtf8 bs
  | otherwise              = Nothing

-- | Unsafely create a modified utf8 string from a 'ByteString'
--
-- Does not validate the 'ByteString' which may yield unexpected
-- results when using 'unpack' later.
unsafeMkModifiedUtf8 :: ByteString -> ModifiedUtf8
{-# INLINE unsafeMkModifiedUtf8 #-}
unsafeMkModifiedUtf8 = ModifiedUtf8

-- | Check if a 'ByteString' is valid modified utf8 
isValidModifiedUtf8 :: ByteString -> Bool
{-# INLINE isValidModifiedUtf8 #-}
isValidModifiedUtf8 (BS.BS fptr len) = BS.accursedUnutterablePerformIO $ BS.unsafeWithForeignPtr fptr $ \ptr -> do
  i <- c_is_valid_modified_utf8 ptr (fromIntegral len)
  pure $ i /= 0

foreign import ccall unsafe "is_valid_modified_utf8" c_is_valid_modified_utf8 :: Ptr Word8 -> CSize -> IO CInt
