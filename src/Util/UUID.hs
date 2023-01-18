{-# LANGUAGE UndecidableInstances #-}
module Util.UUID (
  UUID
, nil
) where

import qualified Data.UUID

import Foreign.Storable

import Util.Binary

import Hecs

newtype UUID = UUID (Data.UUID.UUID)
  deriving newtype (Show, ToBinary, FromBinary, Storable)

nil :: UUID
nil = UUID (Data.UUID.nil)

deriving via ViaStorable UUID instance Component UUID
