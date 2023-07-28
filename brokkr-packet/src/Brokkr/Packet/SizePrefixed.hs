{-# LANGUAGE MagicHash #-}
module Brokkr.Packet.SizePrefixed (
  Sized(..)
, SizePrefixed(..)
, FixedSizeByteString(..), mkFixedSizeByteString
) where

import Brokkr.Packet.Binary

import Control.Monad (when)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Data.Proxy

import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Data.Vector.Storable qualified as S

import FlatParse.Basic qualified as Flatparse

import GHC.Exts
import GHC.TypeLits

newtype SizePrefixed prefix a = SizePrefixed a

class Sized a where
  size :: a -> Int
  withSize :: Int -> (a -> Flatparse.ParserT st PacketParseError r) -> Flatparse.ParserT st PacketParseError r

instance (Num prefix, ToBinary prefix, Sized a, ToBinary a) => ToBinary (SizePrefixed prefix a) where
  put (SizePrefixed a) =
    let sz = size a
    in put @prefix (fromIntegral sz) <> put a
  {-# INLINE put #-}

instance (Integral prefix, FromBinary prefix, Sized a) => FromBinary (SizePrefixed prefix a) where
  with f = with @prefix $ \sz -> withSize (fromIntegral sz) (f . SizePrefixed)
  {-# INLINE with #-}

instance Sized ByteString where
  size = BS.length
  {-# INLINE size #-}
  withSize sz@(I# sz#) f
    | sz < 0    = Flatparse.err $ InvalidArraySize sz
    | otherwise = Flatparse.takeUnsafe# sz# >>= f
  {-# INLINE withSize #-}

instance FromBinary a => Sized (V.Vector a) where
  size = V.length
  {-# INLINE size #-}
  withSize sz f = V.replicateM sz get >>= f
  {-# INLINE withSize #-}

instance (U.Unbox a, FromBinary a) => Sized (U.Vector a) where
  size = U.length
  {-# INLINE size #-}
  withSize sz f = U.replicateM sz get >>= f
  {-# INLINE withSize #-}

instance S.Storable a => Sized (S.Vector a) where
  size = S.length
  {-# INLINE size #-}
  withSize sz f = do
    BS.BS fp _ <- Flatparse.take sz
    f $ S.unsafeFromForeignPtr0 (coerce fp) sz
  {-# INLINE withSize #-}

newtype FixedSizeByteString (sz :: Nat) = UnsafeFixedSizeByteString { unFixedSizeByteString :: ByteString }
  deriving newtype ToBinary

instance KnownNat sz => FromBinary (FixedSizeByteString sz) where
  with f = do
    let !sz@(I# sz#) = fromIntegral $ natVal (Proxy @sz)
    res <- Flatparse.takeUnsafe# sz#
    when (BS.length res /= sz) $ Flatparse.err $ InvalidFixedArraySize sz (BS.length res)
    f $ UnsafeFixedSizeByteString res
  {-# INLINE with #-}

mkFixedSizeByteString :: forall sz . KnownNat sz => ByteString -> Maybe (FixedSizeByteString sz)
mkFixedSizeByteString bs
  | BS.length bs /= fromIntegral (natVal (Proxy @sz)) = Nothing
  | otherwise = Just $ UnsafeFixedSizeByteString bs
