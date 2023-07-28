{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
-- {-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-local-binds -Wno-unused-matches #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Brokkr.BlockState.Internal.Conversion (
  propsToId
, hashProps
, idToProps
, HighestBlockStateId
) where

import Brokkr.BlockState.Internal.TH

genPaletteMapping
