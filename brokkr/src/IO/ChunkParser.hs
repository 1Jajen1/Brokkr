{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module IO.ChunkParser () where

import Brokkr.NBT

import Data.Void

import qualified FlatParse.Basic as FP

import IO.Chunk

import Util.Binary

instance FromBinary Chunk where
  get = voidError chunkParser
  {-# INLINE get #-}

chunkParser :: FP.Parser NBTError Chunk
{-# INLINE chunkParser #-}
chunkParser = $$(genParser $ codec @Chunk)

-- TODO Add error handling to fromBinary
voidError :: Show e => FP.ParserT st e a -> FP.ParserT st Void a
voidError (FP.ParserT g) = FP.ParserT $ \fp eob s st ->
  case g fp eob s st of
    FP.OK# st' a s' -> FP.OK# st' a s'
    FP.Fail# st' -> FP.Fail# st'
    FP.Err# st' e -> error $ show e -- FP.Fail# st'
{-# INLINE voidError #-} 
