{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans  #-}
module Network.Packet.Client.Play.JoinGame (
  JoinGameData(..)
, IsHardcore(..)
, PrevGamemode(..)
, DimensionCodec(..)
, DimensionType(..)
, DimensionRegistryEntry(..)
, BiomeRegistry(..)
, BiomeRegistryEntry(..)
, HashedSeed(..)
, MaxPlayers(..)
, ViewDistance(..)
, ReducedDebugInfo(..)
, EnableRespawnScreen(..)
, IsDebug(..)
, IsFlat(..)
) where

import qualified Data.Vector as V
import Util.Binary
import Data.Int
import Util.NBT
import Data.Text
import Entity.Internal
import Registry.Dimension
import Registry.BiomeSettings
import Network.Util.VarNum
import World.Internal
import Player.GameMode
import Network.Util.FromIntegral
import Data.Coerce

-- TODO Redesign datatypes
data JoinGameData = JoinGameData {
  _entityId            :: EntityId
, _isHardcore          :: IsHardcore
, _gamemode            :: GameMode
, _prevGamemode        :: PrevGamemode
, _worlds              :: V.Vector WorldName
, _dimensionCodec      :: DimensionCodec
, _dimension           :: DimensionSettings
, _worldName           :: WorldName
, _hashedSeed          :: HashedSeed
, _maxPlayers          :: MaxPlayers
, _viewDistance        :: ViewDistance
, _reducedDebugInfo    :: ReducedDebugInfo
, _enableRespawnScreen :: EnableRespawnScreen
, _isDebug             :: IsDebug
, _isFlat              :: IsFlat
}
  deriving stock (Show, Eq)

instance ToBinary JoinGameData where
  put JoinGameData{..} =
       put _entityId
    <> put _isHardcore
    <> put _gamemode
    <> put _prevGamemode
    <> put (SizePrefixed @VarInt _worlds)
    <> put _dimensionCodec
    <> put _dimension
    <> put _worldName
    <> put _hashedSeed
    <> put _maxPlayers
    <> put _viewDistance
    <> put _reducedDebugInfo
    <> put _enableRespawnScreen
    <> put _isDebug
    <> put _isFlat
    <> put False -- TODO Why? Context: I am missing exactly one byte somewhere in this packet, adding one byte just seems to work somehow...
  {-# INLINE put #-}

newtype IsHardcore = IsHardcore Bool
  deriving stock Show
  deriving newtype (Eq, ToBinary)

data PrevGamemode = Undefined | Mode GameMode
  deriving stock (Show, Eq)

instance ToBinary PrevGamemode where
  put Undefined = put @Int8 (-1)
  put (Mode Survival) = put @Int8 0
  put (Mode Creative) = put @Int8 1
  put (Mode Adventure) = put @Int8 2
  put (Mode Spectator) = put @Int8 3

newtype HashedSeed = HashedSeed Int64
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

newtype MaxPlayers = MaxPlayers Int
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via FromIntegral Int VarInt

newtype ViewDistance = ViewDistance Int
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via FromIntegral Int VarInt

newtype ReducedDebugInfo = ReducedDebugInfo Bool
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

newtype EnableRespawnScreen = EnableRespawnScreen Bool
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

newtype IsDebug = IsDebug Bool
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

newtype IsFlat = IsFlat Bool
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

-- Registry stuff
data DimensionCodec = DimensionCodec {
  _dimensionType :: DimensionType
, _worldgenBiome :: BiomeRegistry
}
  deriving stock (Show, Eq)
  deriving ToBinary via BinaryNBT DimensionCodec

instance ToNBT DimensionCodec where
  toNBT DimensionCodec{..} = compound [dimRegNBT, biomeRegNBT]
    where dimRegNBT = ("minecraft:dimension_type" .= _dimensionType)
          biomeRegNBT = ("minecraft:worldgen/biome" .= _worldgenBiome)

data DimensionType = DimensionType {
  _dimtype  :: Text
, _dimvalue :: V.Vector DimensionRegistryEntry
}
  deriving stock (Show, Eq)

instance ToNBT DimensionType where
  toNBT DimensionType{..} =
    compound [
        ("type" .= _dimtype)
      , ("value" .= _dimvalue)
      ]

data DimensionRegistryEntry = DimensionRegistryEntry {
  _dimname    :: Text
, _dimid      :: Int
, _dimelement :: DimensionSettings
}
  deriving stock (Show, Eq)

instance ToNBT DimensionRegistryEntry where
  toNBT DimensionRegistryEntry{..} =
    compound [
        ("name" .= _dimname)
      , ("id" .= fromIntegral @_ @Int32 _dimid)
      , ("element" .= _dimelement)
      ]

deriving via BinaryNBT DimensionSettings instance ToBinary DimensionSettings

instance ToNBT DimensionSettings where
  toNBT DimensionSettings{..} =
    compound $ [
        ("piglin_safe" .= coerce @_ @Bool _piglinSafe)
      , ("natural" .= coerce @_ @Bool _natural)
      , ("ambient_light" .= coerce @_ @Float _ambientLight)
      , ("infiniburn" .= coerce @_ @Text _infiniburn)
      , ("respawn_anchor_works" .= coerce @_ @Bool _respawnAnchorWorks)
      , ("has_skylight" .= coerce @_ @Bool _hasSkylight)
      , ("bed_works" .= coerce @_ @Bool _bedWorks)
      , ("effects" .= coerce @_ @Text _effects)
      , ("has_raids" .= coerce @_ @Bool _hasRaids)
      , ("min_y" .= coerce @_ @Int32 _minY)
      , ("height" .= coerce @_ @Int32 _height)
      , ("logical_height" .= coerce @_ @Int32 _logicalHeight)
      , ("coordinate_scale" .= coerce @_ @Double _coordinateScale) -- Double or Float?
      , ("ultrawarm" .= coerce @_ @Bool _ultrawarm)
      , ("has_ceiling" .= coerce @_ @Bool _hasCeiling)
      ] <> (maybe [] (\x -> [x]) $ fmap (\el -> ("fixed_time" .= coerce @_ @Int64 el)) _fixedTime)

data BiomeRegistry = BiomeRegistry {
  _biometype  :: Text
, _biomevalue :: V.Vector BiomeRegistryEntry
}
  deriving stock (Show, Eq)

instance ToNBT BiomeRegistry where
  toNBT BiomeRegistry{..} =
    compound [
        ("type" .= _biometype)
      , ("value" .= _biomevalue)
    ]

data BiomeRegistryEntry = BiomeRegistryEntry {
  _biomename    :: Text
, _biomeid      :: Int
, _biomeelement :: BiomeSettings
}
  deriving stock (Show, Eq)

instance ToNBT BiomeRegistryEntry where
  toNBT BiomeRegistryEntry{..} =
    compound [
        ("name" .= _biomename)
      , ("id" .= fromIntegral @_ @Int32 _biomeid)
      , ("element" .= _biomeelement)
      ]
  
instance ToNBT BiomeSettings where
  toNBT BiomeSettings{..} =
    compound $ [
        ("precipitation" .= _precipitation)
      , ("temperature" .= coerce @_ @Float _temperature)
      , ("downfall" .= coerce @_ @Float _downfall)
      , ("category" .= _category)
      , ("effects" .= _effects)
      -- , ("particle" .= _particle)
      ] <> (maybe [] (\x -> [x]) $ fmap (\el -> ("temperature_modifer" .= el)) _temperatureModifier)

instance ToNBT Precipitation where
  toNBT Rain = toNBT @Text "rain"
  toNBT Snow = toNBT @Text "snow"
  toNBT None = toNBT @Text "none"

instance ToNBT BiomeCategory where
  toNBT OceanC = toNBT @Text "ocean"
  toNBT PlainsC = toNBT @Text "plains"
  toNBT DesertC = toNBT @Text "desert"
  toNBT ExtremeHillsC = toNBT @Text "extreme_hills"
  toNBT TaigaC = toNBT @Text "taiga"
  toNBT SwampC = toNBT @Text "swamp"
  toNBT RiverC = toNBT @Text "river"
  toNBT NetherC = toNBT @Text "nether"
  toNBT TheEndC = toNBT @Text "the_end"
  toNBT IcyC = toNBT @Text "icy"
  toNBT MushroomC = toNBT @Text "mushroom"
  toNBT BeachC = toNBT @Text "beach"
  toNBT JungleC = toNBT @Text "jungle"
  toNBT MesaC = toNBT @Text "mesa"
  toNBT SavannaC = toNBT @Text "savanna"
  toNBT ForestC = toNBT @Text "forest"
  toNBT UndergroundC = toNBT @Text "underground"
  toNBT MountainC = toNBT @Text "mountain"
  toNBT NoneC = toNBT @Text "none"

instance ToNBT TemperatureModifier where
  toNBT Frozen = toNBT @Text "frozen"

instance ToNBT BiomeEffects where
  toNBT BiomeEffects{..} =
    compound $ [
        ("sky_color" .= coerce @_ @Int32 _skyColor)
      , ("water_fog_color" .= coerce @_ @Int32 _waterFogColor)
      , ("fog_color" .= coerce @_ @Int32 _fogColor)
      , ("water_color" .= coerce @_ @Int32 _waterColor)
      ]
