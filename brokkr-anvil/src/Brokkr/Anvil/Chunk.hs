{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Brokkr.Anvil.Chunk (
  Chunk(..)
, ChunkPosition(..)
, DataVersion(..)
, LowestYSection(..)
, HeightMaps(..)
, HeightMap(..)
, LastUpdate(..)
, FluidTicks(..)
, BlockTicks(..)
, InhabitedTime(..)
, TileTick(..)
) where

import Brokkr.Anvil.Chunk.Section

import Brokkr.NBT
import Brokkr.NBT.Codec
import Brokkr.NBT.Class qualified as Class

import Brokkr.PackedVector (PackedVector, DynamicNat(..))
import Brokkr.PackedVector.Internal qualified as PV

import Data.Maybe
import Data.Coerce (coerce)
import Data.Functor
import Data.Int
import Data.Proxy

import Data.Vector qualified as V
import Data.Vector.Storable qualified as S

import GHC.Base (quotInt)
import GHC.TypeLits

data Chunk = Chunk {
  dataVersion    :: !DataVersion
, chunkPosition  :: !ChunkPosition
, lowestYSection :: !LowestYSection
, chunkSections  :: !(V.Vector ChunkSection)
, heightMaps     :: !HeightMaps
, lastUpdate     :: !LastUpdate
, blockEntities  :: !(V.Vector Tag)
, fluidTicks     :: !FluidTicks
, blockTicks     :: !BlockTicks
, inhabitedTime  :: !InhabitedTime
}
  deriving stock (Eq, Show)

newtype DataVersion = DataVersion Int32
  deriving stock (Eq, Show)
  deriving newtype (Class.FromNBT, Class.ToNBT)

data ChunkPosition = ChunkPosition !Int32 !Int32
  deriving stock (Eq, Show)

newtype LowestYSection = LowestYSection Int32
  deriving stock (Eq, Show)
  deriving newtype (Class.FromNBT, Class.ToNBT)

newtype HeightMaps = HeightMaps {
  motionBlocking :: Maybe HeightMap
}
  deriving stock (Eq, Show)

newtype HeightMap = HeightMap (PackedVector ('Static 256) 'Dynamic Int)
  deriving stock (Eq, Show)

newtype LastUpdate = LastUpdate Int64
  deriving stock (Eq, Show)
  deriving newtype (Class.FromNBT, Class.ToNBT)

newtype FluidTicks = FluidTicks (V.Vector TileTick)
  deriving stock (Eq, Show)
  deriving newtype (Class.FromNBT, Class.ToNBT)

newtype BlockTicks = BlockTicks (V.Vector TileTick)
  deriving stock (Eq, Show)
  deriving newtype (Class.FromNBT, Class.ToNBT)

data TileTick = TileTick {
  tickId       :: !NBTString
, tickPriority :: !Int32
, tickNrTicks  :: !Int32
, tickX        :: !Int32
, tickY        :: !Int32
, tickZ        :: !Int32
}
  deriving stock (Eq, Show)

newtype InhabitedTime = InhabitedTime Int64
  deriving stock (Eq, Show)
  deriving newtype (Class.FromNBT, Class.ToNBT)

instance HasCodec Chunk where
  codec = compound "Chunk" $ [|| Chunk ||]
    <$#> requiredFieldVia @Int32 "DataVersion" .= [|| dataVersion ||]
    <*#> position .= [|| chunkPosition ||]
    <*#> requiredFieldVia @Int32 "yPos" .= [|| lowestYSection ||]
    <*#> requiredField "sections" .= [|| chunkSections ||]
    <*#> requiredField "Heightmaps" .= [|| heightMaps ||]
    <*#> requiredFieldVia @Int64 "LastUpdate" .= [|| lastUpdate ||]
    <*#> requiredField "block_entities" .= [|| blockEntities ||]
    <*#> requiredFieldVia @(V.Vector TileTick) "fluid_ticks" .= [|| fluidTicks ||]
    <*#> requiredFieldVia @(V.Vector TileTick) "block_ticks" .= [|| blockTicks ||]
    <*#> requiredFieldVia @Int64 "InhabitedTime" .= [|| inhabitedTime ||]
    where
      position = [|| ChunkPosition ||]
        <$#> requiredField "xPos" .= [|| \(ChunkPosition x _) -> x ||]
        <*#> requiredField "zPos" .= [|| \(ChunkPosition _ z) -> z ||]

instance HasCodec HeightMaps where
  codec = compound "HeightMaps" $ [|| HeightMaps ||]
    <$#> optionalField "MOTION_BLOCKING" .= [|| motionBlocking ||]

instance HasCodec HeightMap where
  codec = dimapCodec
    [|| \v ->
        let (fp, nrWords) = S.unsafeToForeignPtr0 v
            sz = fromIntegral $ natVal (Proxy @256)
            elsPerWord = (sz + nrWords - 1) `quotInt` nrWords
            bitSz = 64 `quotInt` elsPerWord
        in HeightMap $ PV.unsafeFromForeignPtr @('Static 256) @'Dynamic (coerce fp) bitSz
      ||]
    [|| \(HeightMap pv) ->
      let sz    = PV.length  pv
          bitSz = PV.bitSize pv
          elsPerWord = 64 `quotInt` bitSz
          nrOfWords = (sz + elsPerWord - 1) `quotInt` elsPerWord
      in S.unsafeFromForeignPtr0 (coerce $ PV.unsafeBacking pv) nrOfWords
      ||]
    $ codec @(S.Vector (BigEndian Int64))

instance HasCodec TileTick where
  codec = compound "TileTick" $ [|| TileTick ||]
    <$#> requiredField "i" .= [|| tickId ||]
    <*#> requiredField "p" .= [|| tickPriority ||]
    <*#> requiredField "t" .= [|| tickNrTicks ||]
    <*#> requiredField "x" .= [|| tickX ||]
    <*#> requiredField "y" .= [|| tickY ||]
    <*#> requiredField "z" .= [|| tickZ ||]

instance Class.ToNBT Chunk where
  toNBT Chunk{chunkPosition = ChunkPosition chunkX chunkZ,..} =
    Class.compound [
        "DataVersion" Class..= dataVersion
      , "xPos" Class..= chunkX
      , "zPos" Class..= chunkZ
      , "yPos" Class..= lowestYSection
      , "DataVersion" Class..= chunkSections
      , "DataVersion" Class..= heightMaps
      , "DataVersion" Class..= lastUpdate
      , "DataVersion" Class..= blockEntities
      , "DataVersion" Class..= fluidTicks
      , "DataVersion" Class..= blockTicks
      , "DataVersion" Class..= inhabitedTime
      ]

instance Class.FromNBT Chunk where
  fromNBT _ = Class.withCompound "Chunk" $ \c -> do
    dataVersion <- c Class..: "DataVersion"
    xPos <- c Class..: "xPos"
    zPos <- c Class..: "zPos"
    let chunkPosition = ChunkPosition xPos zPos
    lowestYSection <- c Class..: "yPos"
    chunkSections <- c Class..: "sections"
    heightMaps <- c Class..: "Heightmaps"
    lastUpdate <- c Class..: "LastUpdate"
    blockEntities <- c Class..: "block_entities"
    fluidTicks <- c Class..: "fluid_ticks"
    blockTicks <- c Class..: "block_ticks"
    inhabitedTime <- c Class..: "InhabitedTime"
    pure Chunk{..}

instance Class.ToNBT HeightMaps where
  toNBT HeightMaps{..} =
    Class.compound $ catMaybes [
        ("MOTION_BLOCKING" Class..=) <$> motionBlocking
      ]

instance Class.FromNBT HeightMaps where
  fromNBT _ = Class.withCompound "HeightMaps" $ \c -> do
    motionBlocking <- c Class..:? "MOTION_BLOCKING"
    pure HeightMaps{..}

instance Class.ToNBT HeightMap where
  toNBT (HeightMap pv) =
    let sz    = PV.length  pv
        bitSz = PV.bitSize pv
        elsPerWord = 64 `quotInt` bitSz
        nrOfWords = (sz + elsPerWord - 1) `quotInt` elsPerWord
    in Class.toNBT $ S.unsafeFromForeignPtr0 @(BigEndian Int64) (coerce $ PV.unsafeBacking pv) nrOfWords

instance Class.FromNBT HeightMap where
  fromNBT name t = Class.fromNBT name t <&> \v ->
    let (fp, nrWords) = S.unsafeToForeignPtr0 @(BigEndian Int64) v
        sz = fromIntegral $ natVal (Proxy @256)
        elsPerWord = (sz + nrWords - 1) `quotInt` nrWords
        bitSz = 64 `quotInt` elsPerWord
    in HeightMap $ PV.unsafeFromForeignPtr @('Static 256) @'Dynamic (coerce fp) bitSz

instance Class.ToNBT TileTick where
  toNBT TileTick{..} =
    Class.compound [
        "i" Class..= tickId
      , "p" Class..= tickPriority
      , "t" Class..= tickNrTicks
      , "x" Class..= tickX
      , "y" Class..= tickY
      , "z" Class..= tickZ
      ]

instance Class.FromNBT TileTick where
  fromNBT _ = Class.withCompound "TileTick" $ \c -> do
    tickId <- c Class..: "i"
    tickPriority <- c Class..: "p"
    tickNrTicks <- c Class..: "t"
    tickX <- c Class..: "x"
    tickY <- c Class..: "y"
    tickZ <- c Class..: "z"
    pure TileTick{..}
