{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Dimension (
  Dimension
, entityId
, DimensionType(..)
, new
, DimensionName(..)
, RegionFilePath(..)
) where

import Data.Text (Text)
import Data.String (IsString)

import Network.Util.MCString

import Foreign.Storable

import Util.Binary

import Server
import Hecs

-- TODO Modify and add RegionFileFolderPath to this, so that we don't need too many lookups
-- also they cannot be logically seperate anyway..
newtype Dimension = Dimension EntityId
  deriving newtype (Eq, Storable)
  deriving Component via (ViaFlat Dimension)

entityId :: Dimension -> EntityId
entityId (Dimension eid) = eid
{-# INLINE entityId #-}

data DimensionType = Overworld | Nether | TheEnd

-- TODO Move to common DimensionSettings or something similar
newtype RegionFilePath = RegionFilePath String
  deriving newtype IsString
  deriving Component via (ViaBox RegionFilePath)

-- TODO Specialise
new :: forall (t :: DimensionType) . Has Universe t => RegionFilePath -> EntityId -> Server Dimension
new regionFolder dim = do
  addTag @t dim
  set dim regionFolder
  pure $ Dimension dim

newtype DimensionName = DimensionName Text
  deriving stock Show
  deriving newtype (Eq, IsString)
  deriving (ToBinary, FromBinary) via MCString
  deriving Component via (ViaBox DimensionName)

