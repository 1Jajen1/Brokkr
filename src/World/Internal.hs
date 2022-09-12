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
import qualified IO.Chunkloading as Chunkloading
import IO.ChunkCache ( ChunkCache )
import Command.World
import Util.Queue (Queue)
import World.Dimension
import Optics

data World = World {
  _worldName     :: !WorldName
, _dimension     :: !Dimension
, chunkloading   :: {-# UNPACK #-} !Chunkloading.Handle
, _chunkCache    :: {-# UNPACK #-} !ChunkCache
, _commandQueue  :: Queue Command
}
  deriving stock Show

newtype WorldName = WorldName Text
  deriving stock Show
  deriving newtype (Eq, IsString)
  deriving (ToBinary, FromBinary) via MCString

instance HasDimension World where
  type Access World = A_Getter
  dimension = to _dimension
  {-# INLINE dimension #-}
