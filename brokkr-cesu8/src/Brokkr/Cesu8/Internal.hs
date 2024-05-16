{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module Brokkr.Cesu8.Internal (
  Cesu8(..)
, pack, unpack
, mkCesu8, unsafeMkCesu8
, isValidCesu8
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

-- | ByteString in valid cesu8
newtype Cesu8 = Cesu8 { unwrapCesu8:: ByteString }
  deriving stock Show
  deriving newtype (Eq, Ord, Semigroup, Monoid, NFData)

instance TH.Lift Cesu8 where
  liftTyped (Cesu8 (BS.BS fptr sz)) =
    [|| let !(lit :: Addr#) = $$(TH.unsafeCodeCoerce . TH.litE . TH.bytesPrimL $ TH.mkBytes fptr 0 (fromIntegral sz))
        in Cesu8 (BS.BS (ForeignPtr lit FinalPtr) sz) ||]

instance IsString Cesu8 where
  fromString = pack
  {-# INLINE fromString #-}

-- | Create a cesu8 string from a haskell string
--
-- Surrogate characters are replaced
pack :: String -> Cesu8
{-# INLINE pack #-}
pack = Cesu8 . Common.pack Standard

-- | Create a haskell String from a valid cesu8 string
--
-- Acts as a good producer for list fusion
unpack :: Cesu8 -> String
{-# INLINE unpack #-}
unpack = Common.unpack . coerce

-- | Create a cesu8 string from a 'ByteString'
--
-- Validates the string first and returns 'Nothing' on failure.
mkCesu8 :: ByteString -> Maybe Cesu8
{-# INLINE mkCesu8 #-}
mkCesu8 bs
  | isValidCesu8 bs = Just $ Cesu8 bs
  | otherwise              = Nothing

-- | Unsafely create a cesu8 string from a 'ByteString'
--
-- Does not validate the 'ByteString' which may yield unexpected
-- results when using 'unpack' later.
unsafeMkCesu8 :: ByteString -> Cesu8
{-# INLINE unsafeMkCesu8 #-}
unsafeMkCesu8 = Cesu8

-- | Check if a 'ByteString' is valid cesu8 
isValidCesu8 :: ByteString -> Bool
{-# INLINE isValidCesu8 #-}
isValidCesu8 (BS.BS fptr len) = BS.accursedUnutterablePerformIO $ BS.unsafeWithForeignPtr fptr $ \ptr -> do
  i <- c_is_valid_cesu8 ptr (fromIntegral len)
  pure $ i /= 0

foreign import ccall unsafe "is_valid_cesu8" c_is_valid_cesu8 :: Ptr Word8 -> CSize -> IO CInt
