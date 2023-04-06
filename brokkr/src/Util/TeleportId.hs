module Util.TeleportId (
  TeleportId(..)
) where

import Network.Util
import Util.Binary

newtype TeleportId = TeleportId Int
  deriving stock Show
  deriving newtype Eq
  deriving (FromBinary, ToBinary) via FromIntegral Int VarInt

