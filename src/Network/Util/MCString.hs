module Network.Util.MCString (
  MCString(..)
) where

import Data.Text
import Util.Binary
import Network.Util.SizePrefixed

newtype MCString = MCString Text
  deriving (FromBinary,ToBinary) via (ByteSizePrefixed Text)
