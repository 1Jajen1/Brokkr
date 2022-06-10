{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module World.Internal (
  WorldName(..)
, World(..)
, Dimension(..)
, HasDimension(..)
) where

import Data.Text
import Network.Util.MCString
import Util.Binary
import Data.String
import Optics
import qualified IO.Chunkloading as Chunkloading

data World = World {
  _worldName :: WorldName
, _dimension :: Dimension
, chunkloading :: Chunkloading.Handle
}
  deriving stock Show

newtype WorldName = WorldName Text
  deriving stock Show
  deriving newtype (Eq, IsString)
  deriving (ToBinary, FromBinary) via MCString

data Dimension = Overworld | Nether | TheEnd
  deriving stock (Eq, Enum, Show)

class HasDimension a where
  type Access a :: OpticKind
  dimension :: Optic' (Access a) NoIx a Dimension

