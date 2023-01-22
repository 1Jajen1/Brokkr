module Entity.Util.Hand (
  Hand(..)
) where

import Network.Util

import Util.Binary

data Hand = Main | Off
  deriving stock (Show, Eq)

instance FromBinary Hand where
  get = get @VarInt >>= \case
    0 -> pure Main
    1 -> pure Off

