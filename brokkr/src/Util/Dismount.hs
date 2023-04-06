module Util.Dismount (
  Dismount(..)
) where
import Util.Binary

data Dismount = Dismount | NoDismount
  deriving stock (Eq, Show)

instance ToBinary Dismount where
  put Dismount = put True
  put NoDismount = put False

instance FromBinary Dismount where
  get = get >>= \case
    True -> pure Dismount
    False -> pure NoDismount
