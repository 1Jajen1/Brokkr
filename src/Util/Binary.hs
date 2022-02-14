module Util.Binary (
  FromBinary(..)
, ToBinary(..)
, SizePrefixed(..)
) where

import Data.Kind
import Data.Void
import FlatParse.Basic hiding (anyWord16, anyWord32)
import Data.Int
import Data.Text
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.Float
import Util.Flatparse
import Util.ByteOrder
import qualified Mason.Builder as B
import Data.UUID
import qualified Data.Vector.Storable as S
import qualified Foreign.Storable as S
import qualified Data.ByteString.Internal as BS
import Data.Coerce

-- TODO Test instances, just test roundtripping for now, add regression tests if something fails later on.

class FromBinary (a :: Type) where
  get :: Parser Void a

class ToBinary (a :: Type) where
  put :: a -> B.Builder

-- TODO Roundtrip tests for all instances.
instance FromBinary Int8 where
  get = fromIntegral <$> anyWord8
  {-# INLINE get #-}

instance ToBinary Int8 where
  put = B.int8
  {-# INLINE put #-}

instance FromBinary Int16 where
  get = fromIntegral <$> get @Word16
  {-# INLINE get #-}

instance ToBinary Int16 where
  put = B.int16BE
  {-# INLINE put #-}

instance FromBinary Int32 where
  get = fromIntegral <$> get @Word32
  {-# INLINE get #-}

instance ToBinary Int32 where
  put = B.int32BE
  {-# INLINE put #-}

instance FromBinary Int64 where
  get = fromIntegral <$> get @Word64
  {-# INLINE get #-}

instance ToBinary Int64 where
  put = B.int64BE
  {-# INLINE put #-}

instance FromBinary Word8 where
  get = anyWord8
  {-# INLINE get #-}

instance ToBinary Word8 where
  put = B.word8
  {-# INLINE put #-}

instance FromBinary Word16 where
  get = toBE <$> anyWord16
  {-# INLINE get #-}

instance ToBinary Word16 where
  put = B.word16BE
  {-# INLINE put #-}

instance FromBinary Word32 where
  get = toBE <$> anyWord32
  {-# INLINE get #-}

instance ToBinary Word32 where
  put = B.word32BE
  {-# INLINE put #-}

instance FromBinary Word64 where
  get = toBE <$> anyWord64
  {-# INLINE get #-}

instance ToBinary Word64 where
  put = B.word64BE
  {-# INLINE put #-}

instance FromBinary Float where
  get = castWord32ToFloat <$> get @Word32
  {-# INLINE get #-}

instance ToBinary Float where
  put = B.floatBE
  {-# INLINE put #-}

instance FromBinary Double where
  get = castWord64ToDouble <$> get @Word64
  {-# INLINE get #-}

instance ToBinary Double where
  put = B.doubleBE
  {-# INLINE put #-}

instance FromBinary Text where
  get = T.decodeUtf8 <$> takeRemaining
  {-# INLINE get #-}

instance ToBinary Text where
  put = B.encodeUtf8Builder
  {-# INLINE put #-}

instance FromBinary UUID where
  get = fromWords64 <$> get <*> get
  {-# INLINE get #-}

instance ToBinary UUID where
  put uid = case toWords64 uid of
    (w1, w2) -> put w1 <> put w2
  {-# INLINE put #-}

newtype SizePrefixed (pre :: Type) (a :: Type) = SizePrefixed a

-- TODO This retains the entire ByteString that contains the fptr in memory, is this a problem? Are there cases where it is?
instance (FromBinary pre, Integral pre, S.Storable a) => FromBinary (SizePrefixed pre (S.Vector a)) where
  get = do
    len <- fromIntegral <$> get @pre
    let aSz = S.sizeOf @a undefined
    BS.BS fptr _ <- takeN (len * aSz)
    pure . SizePrefixed $ S.unsafeFromForeignPtr0 (coerce fptr) len
  {-# INLINE get #-}

instance (ToBinary pre, Integral pre, S.Storable a) => ToBinary (SizePrefixed pre (S.Vector a)) where
  put (SizePrefixed vec) =
    let (fptr, len) = S.unsafeToForeignPtr0 vec
    in put (fromIntegral @_ @pre len) <> B.byteString (BS.BS (coerce fptr) (len * S.sizeOf @a undefined))
  {-# INLINE put #-}
