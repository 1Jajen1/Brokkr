module Util.Binary (
  FromBinary(..)
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

class FromBinary (a :: Type) where
  get :: Parser Void a

instance FromBinary Int8 where
  get = fromIntegral <$> anyWord8
  {-# INLINE get #-}

instance FromBinary Int16 where
  get = fromIntegral <$> get @Word16
  {-# INLINE get #-}

instance FromBinary Int32 where
  get = fromIntegral <$> get @Word32
  {-# INLINE get #-}

instance FromBinary Int64 where
  get = fromIntegral <$> get @Word64
  {-# INLINE get #-}

instance FromBinary Word8 where
  get = anyWord8
  {-# INLINE get #-}

instance FromBinary Word16 where
  get = toBE <$> anyWord16
  {-# INLINE get #-}

instance FromBinary Word32 where
  get = toBE <$> anyWord32
  {-# INLINE get #-}

instance FromBinary Word64 where
  get = toBE <$> anyWord64
  {-# INLINE get #-}

instance FromBinary Float where
  get = castWord32ToFloat <$> get @Word32
  {-# INLINE get #-}

instance FromBinary Double where
  get = castWord64ToDouble <$> get @Word64
  {-# INLINE get #-}

instance FromBinary Text where
  get = T.decodeUtf8 <$> takeRemaining
  {-# INLINE get #-}
