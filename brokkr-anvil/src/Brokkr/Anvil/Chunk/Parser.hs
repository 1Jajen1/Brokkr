{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Brokkr.Anvil.Chunk.Parser (
  parseChunkNBT
, putChunkNBT
) where

import Brokkr.Anvil.Chunk
import Brokkr.Anvil.Chunk.Section

import Brokkr.NBT.Codec
import Brokkr.NBT.NBTError
import Brokkr.NBT.NBTString.Internal

import FlatParse.Basic

import Mason.Builder

parseChunkNBT :: ParserT st NBTError Chunk
parseChunkNBT = $$(genParser codec)

putChunkNBT :: Chunk -> Builder
putChunkNBT = $(genBuilder @Chunk codec)
{-# INLINE putChunkNBT #-}
