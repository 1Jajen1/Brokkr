{-# LANGUAGE DataKinds #-}
module Effect.Async (
  Async
) where

import Effectful
import System.IO (Handle)
import Effect.IO.File.Effect
import {-# SOURCE #-} Effect.World
import Effect.EntityId (FreshEntityId)

type Async :: [Effect]
type Async = '[
    File Handle
  , WorldManager
  , FreshEntityId
  ]
