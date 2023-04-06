{-# LANGUAGE MagicHash #-}
module Util.Binary (
  FromBinary(..)
, ToBinary(..)
, SizePrefixed(..)
) where

import Brokkr.NBT

import qualified Data.ByteString.Internal as BS

import Data.Coerce
import Data.Int
import Data.Kind

import Data.Text
import Data.Text.Encoding qualified as T

import Data.UUID

import Data.Vector          qualified as V
import Data.Vector.Storable qualified as S

import Data.Void
import Data.Word

import FlatParse.Basic as FP

import Foreign.Storable qualified as S

import GHC.Float

import Mason.Builder qualified as B

import Util.ByteOrder

-- TODO Test instances, just test roundtripping for now, add regression tests if something fails later on.

class FromBinary (a :: Type) where
  get :: Parser Void a

class ToBinary (a :: Type) where
  put :: a -> B.Builder

-- TODO Roundtrip tests for all instances.
instance FromBinary Bool where
  get = (== 1) <$> anyWord8
  {-# INLINE get #-}

instance ToBinary Bool where
  put True = put @Int8 1
  put False = put @Int8 0
  {-# INLINE put #-}

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
  get = T.decodeUtf8 <$> takeRest
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

-- TODO When I have errors on binary instances
instance FromBinary NBT where
  get = voidError parseNBT
  {-# INLINE get #-}

-- TODO Add error handling to fromBinary
voidError :: FP.ParserT st e a -> FP.ParserT st Void a
voidError (FP.ParserT g) = FP.ParserT $ \fp eob s st ->
  case g fp eob s st of
    FP.OK# st' a s' -> FP.OK# st' a s'
    FP.Fail# st' -> FP.Fail# st'
    FP.Err# st' _ -> FP.Fail# st'
{-# INLINE voidError #-} 

instance ToBinary NBT where
  put = putNBT
  {-# INLINE put #-}

newtype SizePrefixed (pre :: Type) (a :: Type) = SizePrefixed a

-- TODO This retains the entire ByteString that contains the fptr in memory, is this a problem? Are there cases where it is?
instance (FromBinary pre, Integral pre, S.Storable a) => FromBinary (SizePrefixed pre (S.Vector a)) where
  get = do
    len <- fromIntegral <$> get @pre
    let aSz = S.sizeOf @a undefined
    BS.BS fptr _ <- FP.take (len * aSz)
    pure . SizePrefixed $ S.unsafeFromForeignPtr0 (coerce fptr) len
  {-# INLINE get #-}

instance (ToBinary pre, Integral pre, S.Storable a) => ToBinary (SizePrefixed pre (S.Vector a)) where
  put (SizePrefixed vec) =
    let (fptr, len) = S.unsafeToForeignPtr0 vec
    in put (fromIntegral @_ @pre len) <> B.byteString (BS.BS (coerce fptr) (len * S.sizeOf @a undefined))
  {-# INLINE put #-}

instance (FromBinary pre, Integral pre, FromBinary a) => FromBinary (SizePrefixed pre (V.Vector a)) where
  get = do
    len <- fromIntegral <$> get @pre
    SizePrefixed <$> V.replicateM len get
  {-# INLINE get #-}

instance (ToBinary pre, Integral pre, ToBinary a) => ToBinary (SizePrefixed pre (V.Vector a)) where
  put (SizePrefixed vec) =
    let len = fromIntegral $ V.length vec
    in put @pre len <> V.foldMap (\x -> put x) vec
  {-# INLINE put #-}

