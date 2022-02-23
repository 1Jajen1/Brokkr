{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-local-binds -Wno-unused-matches #-}
--{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Block.Internal.Conversion (
  idToProps
, propsToId
, HighestBlockStateId
) where

import Block.Internal.BlockState


import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Data.List
import Data.Text (Text)

import Block.Internal.TH

genPaletteMapping
