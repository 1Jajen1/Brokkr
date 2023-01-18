{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Util.Position (
  Position(..)
, pattern Position
, Falling(..)
) where

import Data.Word

import Foreign.Storable

import FlatParse.Basic (empty)

import Util.Binary

import Util.Linear.V3
import Util.Linear.Vector

import Hecs

newtype Position = Pos (V3 Double)
  deriving stock Show
  deriving newtype (Eq, Storable)
  deriving Component via (ViaStorable Position)

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

data Falling = OnGround | Falling
  deriving stock (Show, Eq)
  deriving Component via (ViaBoxed Falling) -- TODO As Tags? If I ever need to iterate only Falling or only OnGround (like in Physics, I may do that)

instance ToBinary Falling where
  put OnGround = put @Word8 1
  put Falling = put @Word8 0

instance FromBinary Falling where
  get = get @Word8 >>= \case
    0 -> pure Falling
    1 -> pure OnGround
    _ -> empty
