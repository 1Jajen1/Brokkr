{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Util.Position (
  Position(..)
, pattern Position
, OnGround(..)
) where

import Chunk.Position

import Data.Word

import FlatParse.Basic (empty)

import Optics

import Util.Binary

import Util.Linear.V3
import Util.Linear.Vector

newtype Position = Pos (V3 Double)
  deriving stock Show
  deriving newtype Eq

deriving newtype instance VectorSpace Double Position

instance Semigroup Position where
  (<>) = (|+|)

pattern Position :: Double -> Double -> Double -> Position
pattern Position x y z = Pos (V3_Double x y z)
{-# COMPLETE Position #-}

instance ToBinary Position where
  put (Position x y z) = put x <> put y <> put z 

instance FromBinary Position where
  get = Position <$> get <*> get <*> get

instance HasChunkPosition Position where
  chunkPosition = to $ \(Position x _ z) -> ChunkPos (floor x `div` 16) (floor z `div` 16)
  {-# INLINE chunkPosition #-}

data OnGround = OnGround | InAir
  deriving stock (Show, Eq)

instance ToBinary OnGround where
  put OnGround = put @Word8 1
  put InAir = put @Word8 0

instance FromBinary OnGround where
  get = get @Word8 >>= \case
    0 -> pure InAir
    1 -> pure OnGround
    _ -> empty
