{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module World.Internal (
  WorldName(..)
, World(..)
, Dimension(..)
, HasDimension(..)
, getOrLoadChunk
, getOrLoadChunks
, chunk
, Hide(..)
) where

import Data.Text
import Network.Util.MCString
import Util.Binary
import Data.String
import Optics
import qualified IO.Chunkloading as Chunkloading
import qualified Data.HashMap.Strict as HM
import Chunk.Internal
import Data.Coerce
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

data World = World {
  _worldName   :: !WorldName
, _dimension   :: !Dimension
, chunkloading :: {-# UNPACK #-} !Chunkloading.Handle
, _chunks      :: !(HM.HashMap ChunkPosition (Hide Chunk)) -- TODO Store in an IntMap and use Z-Encoding for the position to keep them close together
}
  deriving stock Show

-- TODO MOVE
newtype Hide a = Hide a

instance Show (Hide a) where
  show _ = ""

--
newtype WorldName = WorldName Text
  deriving stock Show
  deriving newtype (Eq, IsString)
  deriving (ToBinary, FromBinary) via MCString

data Dimension = Overworld | Nether | TheEnd
  deriving stock (Eq, Enum, Show)

-- methods
-- TODO Add commands to world and add CacheChunk command here
getOrLoadChunk :: World -> ChunkPosition -> (Chunk -> IO ()) -> IO ()
getOrLoadChunk w cp act
  | Just c <- w ^. chunk cp = act c
  | otherwise = Chunkloading.loadChunk (chunkloading w) cp $ \case
    Just c -> act c
    Nothing -> error "Worldgen not implemented"
{-# INLINE getOrLoadChunk #-}

getOrLoadChunks :: World -> V.Vector ChunkPosition -> (Chunk -> IO ()) -> IO ()
getOrLoadChunks w cps act = do
  let (res, toLoad) = V.partitionWith (\cp ->
        case w ^. chunk cp of
          Just c -> Left $ act c
          Nothing -> Right cp
        ) cps
  sequence_ res >> Chunkloading.loadChunks (chunkloading w) toLoad (act . fromMaybe (error "Worldgen not implemented"))
{-# INLINE getOrLoadChunks #-}

-- lenses
cLens :: Lens' World (HM.HashMap ChunkPosition Chunk)
cLens = lens (coerce . _chunks) $ \w nC -> w { _chunks = coerce nC }
{-# INLINE cLens #-}

chunk :: ChunkPosition -> Lens' World (Maybe Chunk)
chunk cp = cLens %  at cp
{-# INLINE chunk #-}

class HasDimension a where
  type Access a :: OpticKind
  dimension :: Optic' (Access a) NoIx a Dimension

