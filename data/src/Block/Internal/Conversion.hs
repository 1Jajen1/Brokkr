{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-local-binds -Wno-unused-matches #-}
--{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Block.Internal.Conversion (
  propsToId
, HighestBlockStateId
) where

import Block.Internal.TH

genPaletteMapping
