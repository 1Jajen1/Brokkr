module Network.Util.EntityId (
  EntityId(..)
) where

import Data.Kind

import Network.Util.FromIntegral
import Network.Util.VarNum

import Util.Binary

-- Yes the minecraft protocol is not consistent how entity ids are transmitted :(
newtype EntityId (rep :: Type) = EntityId Int
  deriving stock Show
  deriving newtype (Eq, Num, Ord, Enum, Real, Integral)
  deriving (ToBinary, FromBinary) via FromIntegral Int rep
