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

newtype BinaryNBT a = BinaryNBT a

instance FromNBT a => FromBinary (BinaryNBT a) where
  get = do
    NBT _ tag <- get @NBT
    runParser (parseNBT tag) (pure . BinaryNBT) FP.empty tag 
  {-# INLINE get #-}
