{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Network.Packet.Server.Play.PlayerAbilities (
  Abilities
, pattern Flying
) where

import Util.Binary
import Data.Word
import Data.Bits

newtype Abilities = Abilities Word
  deriving newtype Show -- TODO

instance FromBinary Abilities where
  get = Abilities . fromIntegral <$> get @Word8
  {-# INLINE get #-}

pattern Flying :: Abilities
pattern Flying <- (\(Abilities w) -> testBit w 2 -> True)
  where
    Flying = Abilities 2
