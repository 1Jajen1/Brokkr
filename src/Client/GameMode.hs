{-# LANGUAGE PatternSynonyms #-}
module Client.GameMode (
  GameMode(..)
) where

import Data.Word

import FlatParse.Basic

import Util.Binary

data GameMode = Survival | Creative | Adventure | Spectator 
  deriving stock (Show, Eq)

instance ToBinary GameMode where
  put Survival = put @Word8 0
  put Creative = put @Word8 1
  put Adventure = put @Word8 2
  put Spectator = put @Word8 3
  {-# INLINE put #-}

instance FromBinary GameMode where
  get = get @Word8 >>= \case
    0 -> pure Survival
    1 -> pure Creative
    2 -> pure Adventure
    3 -> pure Spectator
    _ -> empty
  {-# INLINE get #-}
