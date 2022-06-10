{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Util.Position (
  Position(..)
, pattern Position
, HasPosition(..)
) where

import Util.Linear.V3
import Util.Binary
import Util.Linear.Vector
import Chunk.Position hiding (Access)
import qualified Chunk.Position
import Optics

newtype Position = Pos (V3 Double)
  deriving stock Show
  deriving newtype Eq

deriving newtype instance VectorSpace Double Position

instance Semigroup Position where
  (<>) = (|+|)

pattern Position :: Double -> Double -> Double -> Position
pattern Position x y z = Pos (V3_Double x y z)
{-# COMPLETE Position #-}

class HasPosition a where
  type Access a :: OpticKind
  position :: Optic' (Access a) NoIx a Position

instance ToBinary Position where
  put (Position x y z) = put x <> put y <> put z 

instance FromBinary Position where
  get = Position <$> get <*> get <*> get

instance HasChunkPosition Position where
  type Access Position = A_Getter
  chunkPosition = to $ \(Position x _ z) -> ChunkPos (floor x `div` 16) (floor z `div` 16)
  {-# INLINE chunkPosition #-}
