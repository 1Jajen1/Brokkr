module Util.Disconnect (
  DisconnectReason(..)
) where

import Data.Text (Text)

-- TODO MOve this again, this isn't quite right
data DisconnectReason = ClientDisconnect | ServerDisconnect Text
  deriving stock (Eq, Show)
