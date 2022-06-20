{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module World.Internal (
  WorldName(..)
, World(..)
, Dimension(..)
, HasDimension(..)
, getOrLoadChunk
, chunk
) where

import Data.Text
import Network.Util.MCString
import Util.Binary
import Data.String
import Optics
import qualified IO.Chunkloading as Chunkloading
import qualified Data.HashMap.Strict as HM
import Chunk.Internal

data World = World {
  _worldName   :: !WorldName
, _dimension   :: !Dimension
, chunkloading :: {-# UNPACK #-} !Chunkloading.Handle
, _chunks      :: !(HM.HashMap ChunkPosition Chunk) -- TODO Store in an IntMap and use Z-Encoding for the position to keep them close together
}
  deriving stock Show

newtype WorldName = WorldName Text
  deriving stock Show
  deriving newtype (Eq, IsString)
  deriving (ToBinary, FromBinary) via MCString

data Dimension = Overworld | Nether | TheEnd
  deriving stock (Eq, Enum, Show)

--
getOrLoadChunk :: World -> ChunkPosition -> IO Chunk
getOrLoadChunk w cp
  | Just c <- w ^. chunk cp = pure c
  | otherwise = Chunkloading.loadChunk (chunkloading w) cp >>= \case
    Just c -> pure c
    Nothing -> error "Worldgen not implemented"

-- lenses
cLens :: Lens' World (HM.HashMap ChunkPosition Chunk)
cLens = lens _chunks $ \w nC -> w { _chunks = nC }
{-# INLINE cLens #-}

chunk :: ChunkPosition -> Lens' World (Maybe Chunk)
chunk cp = cLens %  at cp
{-# INLINE chunk #-}

class HasDimension a where
  type Access a :: OpticKind
  dimension :: Optic' (Access a) NoIx a Dimension

