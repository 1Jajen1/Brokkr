{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Inventory.Internal (
  Inventory(..)
-- quality of life 
, Concat, Append, Index, Replicate
, SlotType(..)
, CraftingGridType(..)
, ArmourType(..)
, SomeInventory
, mkSomeInventory
, unsafeFromSomeInventory
) where

import Brokkr.NBT (NBT)

import Data.Coerce
import Data.Int
import Data.Primitive
import GHC.TypeLits

import Hecs (Component, ViaBox)

type family Concat (xs :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (x:xs) = Append x (Concat xs) 

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append xs '[] = xs
  Append (x:xs) ys = x : Append xs ys

type family Replicate (n :: Nat) (a :: k) :: [k] where
  Replicate 0 _ = '[]
  Replicate n a = a : Replicate (n - 1) a

type family Index (n :: Nat) (xs :: [k]) :: k where
  Index _ '[] = TypeError ('Text "Index out of bounds")
  Index 0 (x:_) = x
  Index n (_:xs) = Index (n - 1) xs

-- Some inventory. The actual type needs to be known from context
-- Slightly unsafe. If you cast this to the wrong inventory type it may lead to segfaults due to index out of bounds errors
data SomeInventory = SomeInventory (PrimArray Int32) (PrimArray Int32) (SmallArray (Maybe NBT))
  deriving Component via (ViaBox SomeInventory)

newtype Inventory (slots :: [SlotType]) = Inventory SomeInventory

mkSomeInventory :: Inventory slots -> SomeInventory
mkSomeInventory = coerce

unsafeFromSomeInventory :: SomeInventory -> Inventory slots
unsafeFromSomeInventory = coerce

data SlotType where
  Hotbar :: SlotType
  Main :: SlotType
  Crafting :: CraftingGridType -> SlotType
  Armour :: ArmourType -> SlotType
  OffHand :: SlotType

data CraftingGridType = Input | Output
data ArmourType = Helmet | ChestPlate | Pants | Boots
