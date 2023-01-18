{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Dimension (
  Dimension
, entityId
, Overworld
, Nether
, TheEnd
, new
, DimensionName(..)
, RegionFilePath(..)
) where

import Data.Text (Text)
import Data.String (IsString)

import Network.Util.MCString

import Foreign.Storable

import Util.Binary

import Monad (Universe)

import Hecs
-- TODO These imports aren't nice
import Hecs.World (Has)
import Hecs.Component.Internal (TagBackend)

-- TODO Modify and add RegionFileFolderPath to this, so that we don't need too many lookups
-- also they cannot be logically seperate anyway..
newtype Dimension = Dimension EntityId
  deriving newtype (Eq, Storable)
  deriving Component via (ViaStorable Dimension)

entityId :: Dimension -> EntityId
entityId (Dimension eid) = eid
{-# INLINE entityId #-}

data Overworld deriving Component via (ViaTag Overworld)
data Nether    deriving Component via (ViaTag Nether   )
data TheEnd    deriving Component via (ViaTag TheEnd   )

-- TODO Move to common DimensionSettings or something similar
newtype RegionFilePath = RegionFilePath String
  deriving newtype IsString
  deriving Component via (ViaBoxed RegionFilePath)

new :: forall t m . (Backend t ~ TagBackend, MonadHecs Universe m, Has Universe t) => RegionFilePath -> EntityId -> m Dimension
new regionFolder dim = do
  Hecs.setTag @t dim
  Hecs.setComponent dim regionFolder
  pure $ Dimension dim

newtype DimensionName = DimensionName Text
  deriving stock Show
  deriving newtype (Eq, IsString)
  deriving (ToBinary, FromBinary) via MCString
  deriving Component via (ViaBoxed DimensionName)

