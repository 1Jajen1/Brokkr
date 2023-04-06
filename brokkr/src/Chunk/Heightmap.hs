{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Chunk.Heightmap (
  Heightmaps(..)
, Heightmap(..)
) where

import Brokkr.NBT

import Control.DeepSeq

import Data.Coerce (coerce)

import qualified Data.Vector.Storable as S

import GHC.Generics

import Util.NBT qualified as OldNBT
import Util.Vector.Packed

data Heightmaps = Heightmaps {
  motionBlocking :: Maybe (Heightmap) -- TODO Set defaults
}
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

instance HasCodec Heightmaps where
  codec = compound "heightmaps" $ [|| Heightmaps ||]
    <$#> optionalField "MOTION_BLOCKING" .= [|| motionBlocking ||]

instance OldNBT.FromNBT Heightmaps where
  parseNBT = OldNBT.withCompound $ \obj -> do
    motionBlocking <- obj OldNBT..:? "MOTION_BLOCKING"
    pure Heightmaps{..}
  {-# INLINE parseNBT #-}

instance OldNBT.ToNBT Heightmaps where
  toNBT Heightmaps{..} = case motionBlocking of
    Just hm -> OldNBT.compound ["MOTION_BLOCKING" OldNBT..= hm]
    Nothing -> OldNBT.compound []

newtype Heightmap = Heightmap (PackedVector ('Static 256) ('Static 9))
  deriving newtype (Eq, Show, NFData, OldNBT.FromNBT, OldNBT.ToNBT)

instance HasCodec Heightmap where
  codec = viaCodec $ dimapCodec
    [|| Heightmap . unsafeStaticFromForeignPtr . coerce . fst . S.unsafeToForeignPtr0 @Int64BE ||]
    [|| error "TODO" ||] -- TODO Encoding side of this. Probably after switching to my new packed vector
    codec
