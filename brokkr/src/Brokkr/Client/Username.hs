{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Client.Username (
  Username(..)
, ClientUUID(..)
) where

import Data.Text (Text)
import Data.UUID

import Hecs qualified

-- TODO Enforce username rules so we don't have to recheck on every network send 
newtype Username = Username Text
  deriving stock Show
  deriving newtype Eq
  deriving Hecs.Component via Hecs.ViaBox Text

newtype ClientUUID = ClientUUID UUID
  deriving stock Show
  deriving newtype Eq
  deriving Hecs.Component via Hecs.ViaBox UUID
