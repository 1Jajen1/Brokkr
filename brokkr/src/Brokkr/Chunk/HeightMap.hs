{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Chunk.HeightMap (
  HeightMaps(..)
, HeightMap(..)
, ToHeightMapSize
) where

import Brokkr.NBT
import Brokkr.NBT.Class
import Brokkr.PackedVector.Internal

import Control.DeepSeq

import Data.Coerce (coerce)
import Data.Functor

import qualified Data.Vector.Storable as S

import Data.Proxy

import GHC.Base (quotInt)
import GHC.Generics

import GHC.TypeLits

type ToHeightMapSize dimHeight = Log2 (dimHeight + (2 ^ Log2 dimHeight) - 1)

newtype HeightMaps dimHeight = HeightMaps {
  motionBlocking :: Maybe (HeightMap dimHeight) -- TODO Set defaults
}
  deriving stock (Eq, Generic)
  deriving anyclass NFData

deriving stock instance KnownNat (ToHeightMapSize dimHeight) => Show (HeightMaps dimHeight)

instance KnownNat (ToHeightMapSize dimHeight) => FromNBT (HeightMaps dimHeight) where
  fromNBT name = withCompound name $ \obj -> do
    motionBlocking <- obj .:? "MOTION_BLOCKING"
    pure HeightMaps{..}
  {-# INLINE fromNBT #-}

instance KnownNat (ToHeightMapSize dimHeight) => ToNBT (HeightMaps dimHeight) where
  toNBT HeightMaps{..} = case motionBlocking of
    Just hm -> compound ["MOTION_BLOCKING" .= hm]
    Nothing -> compound []

newtype HeightMap dimHeight = HeightMap (PackedVector ('Static 256) ('Static (ToHeightMapSize dimHeight)) Int)
  deriving newtype (Eq, NFData)

deriving stock instance KnownNat (ToHeightMapSize dimHeight) => Show (HeightMap dimHeight)

instance KnownNat (ToHeightMapSize dimHeight) => FromNBT (HeightMap dimHeight) where
  fromNBT name tag = fromNBT name tag <&>
    HeightMap . unsafeFromForeignPtr @('Static 256) @('Static (ToHeightMapSize dimHeight)) . coerce . fst . S.unsafeToForeignPtr0 @Int64BE

instance KnownNat (ToHeightMapSize dimHeight) => ToNBT (HeightMap dimHeight) where
  toNBT (HeightMap (PVec_SS fp)) =
    let bitSz = fromIntegral $ natVal (Proxy @(ToHeightMapSize dimHeight))
        elsPerWord = 64 `quotInt` bitSz
        sz = fromIntegral $ natVal (Proxy @256)
        nrOfWords = (sz + elsPerWord - 1) `quotInt` elsPerWord
    in toNBT @(S.Vector Int64BE) $ S.unsafeFromForeignPtr0 (coerce fp) nrOfWords
