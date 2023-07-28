module Brokkr.Item.ItemStack (
  ItemStack(..)
) where

newtype ItemType = ItemType Int

data ItemMeta =
    Enchanting
  | Description
  | Name

data ItemStack = ItemStack
  {-# UNPACK #-} !ItemType
  {-# UNPACK #-} !Int
  !ItemMeta
