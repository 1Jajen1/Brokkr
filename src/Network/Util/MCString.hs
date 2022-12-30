module Network.Util.MCString (
  MCString(..)
) where

import Data.Text
import Util.Binary
import Network.Util.SizePrefixed

-- TODO Diff between String, Chat, Identifier
-- Also track the max size in the type and check on creation
-- Allow unsafe creation only from the network
newtype MCString = MCString Text
  deriving (FromBinary,ToBinary) via (ByteSizePrefixed Text)
