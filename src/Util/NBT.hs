-- {-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all  #-}
module Util.NBT (
  NBT
, Tag
, FromNBT(..)
, ToNBT(..)
, withCompound
, (.:), (.:?), (.!=)
, compound, (.=)
, runParser
, BinaryNBT(..)
) where

import Util.NBT.Internal
import Util.Binary
import qualified FlatParse.Basic as FP
-- import Util.NBT.Packed

newtype BinaryNBT a = BinaryNBT a

instance FromNBT a => FromBinary (BinaryNBT a) where
  get = do
    NBT _ tag <- get @NBT
    runParser (parseNBT tag) (pure . BinaryNBT) FP.empty tag 
  {-# INLINE get #-}

instance ToNBT a => ToBinary (BinaryNBT a) where
  put (BinaryNBT a) = put $ NBT "" $ toNBT a
  {-# INLINE put #-}


-- test = $$(mkParser $ CompoundCodec $ RequiredKeyCodec "" $ ListCodec (Just 20) IntCodec)
