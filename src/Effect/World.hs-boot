{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
module Effect.World (
  WorldManager
) where

import Effectful

type role WorldManager phantom phantom
data WorldManager :: Effect
