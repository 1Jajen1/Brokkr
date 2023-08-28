{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-splices -fforce-recomp #-}
module Brokkr.NBT.Codec (
  NBTCodec
, CodecContext(..)
, Code
-- api
, dimapCodec
, rmapCodec
, rmapEitherCodec
, lmapCodec
, (<$#>)
, pureC
, (<*#>)
-- class
, HasCodec(..)
, ViaSizedInt(..)
, ViaList(..)
-- combinators
, compound
, requiredField
, requiredFieldVia
, optionalField
, optionalFieldVia
, (.=)
, utf8String
, unsafeIntArray
, unsafeLongArray
-- gen
, genParser
, genBuilder
-- , genNBTReader
-- , genBuilder
-- , genNBTWriter
) where

import Brokkr.NBT.Codec.Internal
import Brokkr.NBT.Codec.DecodeBinary
import Brokkr.NBT.Codec.EncodeBinary
import Brokkr.NBT.Codec.Show

instance Show (NBTCodec ctx i o) where
  showsPrec = showsPrecCodec
