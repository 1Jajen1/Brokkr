module Network.Util (
  module Network.Util.EntityId
, module Network.Util.FromIntegral
, module Network.Util.VarNum
, module Network.Util.MCString
, module Network.Util.Slot
) where

import Data.Bitfield (Bitfield(Bitfield))

import Network.Util.EntityId
import Network.Util.FromIntegral
import Network.Util.VarNum
import Network.Util.MCString
import Network.Util.Slot

import Util.Binary

instance FromBinary rep => FromBinary (Bitfield rep a) where
  get = Bitfield <$> get
