module Network.Util.FromIntegral (
  FromIntegral(..)
) where

import Data.Kind
import Util.Binary

newtype FromIntegral (from :: Type) (to :: Type) = FromIntegral from

instance (Num from, Integral to, FromBinary to) => FromBinary (FromIntegral from to) where
  get = FromIntegral . fromIntegral <$> get @to
  {-# INLINE get #-}

instance (Integral from, Num to, ToBinary to) => ToBinary (FromIntegral from to) where
  put (FromIntegral a) = put @to $ fromIntegral a
  {-# INLINE put #-}
