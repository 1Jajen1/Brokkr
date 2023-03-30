module Util.Angle (
  Angle(..)
) where

import Data.Word
import Util.Binary

newtype Angle = Angle Word8
  deriving stock Show
  deriving newtype (Eq, FromBinary, ToBinary)
