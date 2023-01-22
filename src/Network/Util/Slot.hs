module Network.Util.Slot (
  SlotIndex(..)
, Slot(..)
) where

import Data.Int
import Data.Word

import FlatParse.Basic (lookahead)

import Network.Util.FromIntegral
import Network.Util.VarNum

import Util.Binary
import Util.NBT

newtype SlotIndex = SlotIndex Int16
  deriving stock Show
  deriving newtype (Eq, FromBinary, ToBinary)

-- TODO Autogen and newtype again for Binary instances
newtype ItemId = ItemId Int
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via (FromIntegral Int VarInt)

newtype ItemCount = ItemCount Word8
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

data Slot =
    EmptySlot
  | ItemSlot !ItemId !ItemCount !(Maybe NBT)
  deriving stock (Show, Eq)

instance FromBinary Slot where
  get = get >>= \case
    False -> pure EmptySlot
    True -> do
      iId <- get
      iC  <- get

      iNBT <- lookahead (get @Word8) >>= \case
        0 -> get @Word8 >> pure Nothing
        _ -> Just <$> get

      pure $ ItemSlot iId iC iNBT
