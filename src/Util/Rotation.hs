{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Util.Rotation (
  Rotation(..)
, pattern Rotation
, HasRotation(..)
) where
import Util.Linear.V2
import Util.Binary
import Util.Linear.Vector
import Optics

newtype Rotation = Rot (V2 Float)
  deriving stock Show
  deriving newtype Eq

deriving newtype instance VectorSpace Float Rotation

instance Semigroup Rotation where
  (<>) = (|+|)

pattern Rotation :: Float -> Float -> Rotation
pattern Rotation yaw pitch = Rot (V2_Float yaw pitch)
{-# COMPLETE Rotation #-}

instance ToBinary Rotation where
  put (Rotation yaw pitch) = put yaw <> put pitch

instance FromBinary Rotation where
  get = Rotation <$> get <*> get

class HasRotation a where
  type Access a :: OpticKind
  rotation :: Optic' (Access a) NoIx a Rotation
