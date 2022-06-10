module Player.GameMode (
  GameMode(..)
) where
import Util.Binary
import Data.Word

data GameMode =
    Survival
  | Creative
  | Adventure
  | Spectator
  deriving stock (Eq, Show)

instance ToBinary GameMode where
  put Survival = put @Word8 0
  put Creative = put @Word8 1
  put Adventure = put @Word8 2
  put Spectator = put @Word8 3
