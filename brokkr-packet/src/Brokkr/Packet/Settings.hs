{-# LANGUAGE DataKinds #-}
module Brokkr.Packet.Settings (
  CompressionSettings(..)
, EncryptionSettings(..)
, KnownCompression(..)
, SomeCompression
) where
import GHC.TypeLits
import Data.Proxy

data CompressionSettings = UseCompression !Nat | NoCompression

data EncryptionSettings = UseEncryption | NoEncryption

class KnownCompression compression where
  compressionVal :: Proxy compression -> CompressionSettings -> CompressionSettings

data SomeCompression

instance KnownCompression SomeCompression where
  compressionVal _ cs = cs
  {-# INLINE compressionVal #-}

instance KnownNat thresh => KnownCompression (UseCompression thresh) where
  compressionVal _ _ = UseCompression (fromIntegral $ natVal (Proxy @thresh))
  {-# INLINE compressionVal #-}

instance KnownCompression NoCompression where
  compressionVal _ _ = NoCompression
  {-# INLINE compressionVal #-}
