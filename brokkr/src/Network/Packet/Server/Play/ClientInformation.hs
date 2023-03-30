module Network.Packet.Server.Play.ClientInformation (
  Locale(..)
, ViewDistance(..)
, ChatMode(..)
, UseChatColors(..)
, DisplayedSkinParts(..)
, MainHand(..)
, UseTextFiltering(..)
, AllowServerListings(..)
) where

import Data.Text hiding (empty)
import Data.Word

import FlatParse.Basic

import Network.Util.MCString
import Network.Util.VarNum

import Util.Binary

newtype Locale = Locale Text
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via MCString

newtype ViewDistance = ViewDistance Word8
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

data ChatMode = Enabled | CommandOnly | Hidden
  deriving stock (Show, Eq)

instance ToBinary ChatMode where
  put Enabled = put (VarInt 0)
  put CommandOnly = put (VarInt 1)
  put Hidden = put (VarInt 2)

instance FromBinary ChatMode where
  get = get >>= \case
    VarInt 0 -> pure Enabled
    VarInt 1 -> pure CommandOnly
    VarInt 2 -> pure Hidden
    -- TODO Handle error case

newtype UseChatColors = UseChatColors Bool
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

newtype DisplayedSkinParts = DisplayedSkinParts Word8
  deriving stock Show -- TODO Custom show instance and pattern synonyms
  deriving newtype (Eq, ToBinary, FromBinary)

data MainHand = LeftHand | RightHand
  deriving stock (Show, Eq)

instance ToBinary MainHand where
  put LeftHand = put (VarInt 0)
  put RightHand = put (VarInt 1)

instance FromBinary MainHand where
  get = get >>= \case
    VarInt 0 -> pure LeftHand
    VarInt 1 -> pure RightHand
    -- TODO Handle error case

newtype UseTextFiltering = UseTextFiltering Bool
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

newtype AllowServerListings = AllowServerListings Bool
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)
