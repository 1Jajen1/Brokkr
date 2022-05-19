{-# LANGUAGE PatternSynonyms #-}
module Util.Rotation (
  Rotation(..)
, pattern Rotation
) where
import Util.Linear.V2
import Util.Binary

newtype Rotation = Rot (V2 Float)
  deriving stock Show

pattern Rotation :: Float -> Float -> Rotation
pattern Rotation yaw pitch = Rot (V2_Float yaw pitch)
{-# COMPLETE Rotation #-}

instance ToBinary Rotation where
  put (Rotation yaw pitch) = put yaw <> put pitch
