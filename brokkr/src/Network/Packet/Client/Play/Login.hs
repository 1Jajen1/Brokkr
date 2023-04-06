{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans  #-}
module Network.Packet.Client.Play.Login (
  LoginData(..)
, IsHardcore(..)
, PrevGamemode(..)
, RegistryCodec(..)
, DimensionType(..)
, DimensionTypeRegistry(..)
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
, ChatRegistry(..)
, SimulationDistance(..)
, DeathLocation(..)
) where

import Brokkr.NBT.Internal (Tag(TagInt, TagString))
import qualified Data.Text as T
import qualified Data.Vector as V
import Util.Binary
import Data.Int
import Util.NBT
import Data.Text
import Brokkr.Registry.Dimension
import Brokkr.Registry.BiomeSettings
import Network.Util.VarNum
import Dimension hiding (DimensionType)
import Client.GameMode
import Network.Util
import Data.Coerce
import Util.Position
import Data.String (IsString(..))
import FlatParse.Basic qualified as FP
-- TODO

-- TODO Redesign datatypes
-- TODO Load registries from json ...
data LoginData = LoginData {
  _entityId            :: EntityId Int32
, _isHardcore          :: IsHardcore
, _gamemode            :: GameMode
, _prevGamemode        :: PrevGamemode
, _dimNames            :: V.Vector DimensionName
, _registryCodec       :: RegistryCodec
, _dimType             :: DimensionType
, _dimName             :: DimensionName
, _hashedSeed          :: HashedSeed
, _maxPlayers          :: MaxPlayers
, _viewDistance        :: ViewDistance
, _simulationDistance  :: SimulationDistance
, _reducedDebugInfo    :: ReducedDebugInfo
, _enableRespawnScreen :: EnableRespawnScreen
, _isDebug             :: IsDebug
, _isFlat              :: IsFlat
, _deathLocation       :: DeathLocation
}
  deriving stock (Show, Eq)

instance ToBinary LoginData where
  put LoginData{..} =
       put _entityId
    <> put _isHardcore
    <> put _gamemode
    <> put _prevGamemode
    <> put (SizePrefixed @VarInt _dimNames)
    <> put _registryCodec
    <> put _dimType
    <> put _dimName
    <> put _hashedSeed
    <> put _maxPlayers
    <> put _viewDistance
    <> put _simulationDistance
    <> put _reducedDebugInfo
    <> put _enableRespawnScreen
    <> put _isDebug
    <> put _isFlat
    <> put _deathLocation
  {-# INLINE put #-}

instance FromBinary LoginData where
  get = LoginData
    <$> get
    <*> get
    <*> get
    <*> get
    <*> (coerce <$> get @(SizePrefixed VarInt (V.Vector DimensionName)))
    <*> get
    <*> get
    <*> get
    <*> get
    <*> get
    <*> get
    <*> get
    <*> get
    <*> get
    <*> get
    <*> get
    <*> get

newtype IsHardcore = IsHardcore Bool
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

data PrevGamemode = Undefined | Mode GameMode
  deriving stock (Show, Eq)

instance ToBinary PrevGamemode where
  put Undefined = put @Int8 (-1)
  put (Mode Survival) = put @Int8 0
  put (Mode Creative) = put @Int8 1
  put (Mode Adventure) = put @Int8 2
  put (Mode Spectator) = put @Int8 3

instance FromBinary PrevGamemode where
  get = get @Int8 >>= \case
    -1 -> pure Undefined
    0 -> pure $ Mode Survival
    1 -> pure $ Mode Creative
    2 -> pure $ Mode Adventure
    3 -> pure $ Mode Spectator
    _ -> FP.empty

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

newtype SimulationDistance = SimulationDistance Int
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

data DeathLocation = NoDeathLocation | DeathLocation DimensionName Position
  deriving stock (Show, Eq)

instance ToBinary DeathLocation where
  put NoDeathLocation = put False
  put (DeathLocation dim pos) = put True <> put dim <> put pos

instance FromBinary DeathLocation where
  get = get >>= \case
    False -> pure NoDeathLocation
    True -> DeathLocation <$> get <*> get

newtype DimensionType = DimensionType Text
  deriving stock Show
  deriving newtype (Eq, IsString, ToNBT, FromNBT)
  deriving (ToBinary, FromBinary) via MCString

-- Registry stuff
data RegistryCodec = RegistryCodec {
  _dimensionType :: DimensionTypeRegistry
, _worldgenBiome :: BiomeRegistry
, _chatType      :: ChatRegistry 
}
  deriving stock (Show, Eq)
  deriving (FromBinary, ToBinary) via BinaryNBT RegistryCodec

instance FromNBT RegistryCodec where
  parseNBT = withCompound $ \t -> do
    _dimensionType <- t .: "minecraft:dimension_type"
    _worldgenBiome <- t .: "minecraft:worldgen/biome"
    _chatType      <- t .: "minecraft:chat_type"
    pure RegistryCodec{..}

instance ToNBT RegistryCodec where
  toNBT RegistryCodec{..} = compound [dimRegNBT, biomeRegNBT, chatRegNBT]
    where dimRegNBT   = "minecraft:dimension_type" .= _dimensionType
          biomeRegNBT = "minecraft:worldgen/biome" .= _worldgenBiome
          chatRegNBT  = "minecraft:chat_type" .= _chatType

data DimensionTypeRegistry = DimensionTypeRegistry {
  _dimtype  :: DimensionType
, _dimvalue :: V.Vector DimensionRegistryEntry
}
  deriving stock (Show, Eq)

instance ToNBT DimensionTypeRegistry where
  toNBT DimensionTypeRegistry{..} =
    compound [
        "type"  .= _dimtype
      , "value" .= _dimvalue
      ]

instance FromNBT DimensionTypeRegistry where
  parseNBT = withCompound $ \t -> do
    DimensionTypeRegistry
      <$> t .: "type"
      <*> t .: "value"

data DimensionRegistryEntry = DimensionRegistryEntry {
  _dimname    :: Text
, _dimid      :: Int
, _dimelement :: DimensionSettings
}
  deriving stock (Show, Eq)

instance ToNBT DimensionRegistryEntry where
  toNBT DimensionRegistryEntry{..} =
    compound [
        "name"    .= _dimname
      , "id"      .= fromIntegral @_ @Int32 _dimid
      , "element" .= _dimelement
      ]

instance FromNBT DimensionRegistryEntry where
  parseNBT = withCompound $ \t ->
    DimensionRegistryEntry
      <$> t .: "name"
      <*> (fromIntegral @Int32 <$> t .: "id")
      <*> t .: "element"

deriving via BinaryNBT DimensionSettings instance ToBinary DimensionSettings
deriving via BinaryNBT DimensionSettings instance FromBinary DimensionSettings

instance ToNBT DimensionSettings where
  toNBT DimensionSettings{..} =
    compound $ [
        "piglin_safe" .= coerce @_ @Bool _piglinSafe
      , "has_raids" .= coerce @_ @Bool _hasRaids
      , "monster_spawn_light_level" .= _monster_spawn_light_level
      , "monster_spawn_block_light_limit" .= coerce @_ @Int32 _monster_spawn_block_light_limit
      , "natural" .= coerce @_ @Bool _natural
      , "ambient_light" .= coerce @_ @Float _ambientLight
      , "infiniburn" .= coerce @_ @Text _infiniburn
      , "respawn_anchor_works" .= coerce @_ @Bool _respawnAnchorWorks
      , "has_skylight" .= coerce @_ @Bool _hasSkylight
      , "bed_works" .= coerce @_ @Bool _bedWorks
      , "effects" .= coerce @_ @Text _effects
      , "min_y" .= coerce @_ @Int32 _minY
      , "height" .= coerce @_ @Int32 _height
      , "logical_height" .= coerce @_ @Int32 _logicalHeight
      , "coordinate_scale" .= coerce @_ @Double _coordinateScale -- Double or Float?
      , "ultrawarm" .= coerce @_ @Bool _ultrawarm
      , "has_ceiling" .= coerce @_ @Bool _hasCeiling
      ] <> maybe [] (\x -> ["fixed_time" .= coerce @_ @Int64 x]) _fixedTime

instance FromNBT DimensionSettings where
  parseNBT = withCompound $ \t -> do
    _piglinSafe <- coerce @Bool <$> t .: "piglin_safe"
    _hasRaids   <- coerce @Bool <$> t .: "has_raids"
    _monster_spawn_light_level       <- t .: "monster_spawn_light_level"
    _monster_spawn_block_light_limit <- coerce @Int32 <$> t .: "monster_spawn_block_light_limit"
    _natural      <- coerce @Bool  <$> t .: "natural"
    _ambientLight <- coerce @Float <$> t .: "ambient_light"
    _infiniburn   <- coerce @Text  <$> t .: "infiniburn"
    _respawnAnchorWorks <- coerce @Bool <$> t .: "respawn_anchor_works"
    _hasSkylight        <- coerce @Bool <$> t .: "has_skylight"
    _bedWorks <- coerce @Bool  <$> t .: "bed_works"
    _effects  <- coerce @Text  <$> t .: "effects"
    _minY     <- coerce @Int32 <$> t .: "min_y"
    _height   <- coerce @Int32 <$> t .: "height"
    _logicalHeight   <- coerce @Int32  <$> t .: "logical_height"
    _coordinateScale <- coerce @Double <$> t .: "coordinate_scale"
    _ultrawarm  <- coerce @Bool  <$> t .: "ultrawarm"
    _hasCeiling <- coerce @Bool  <$> t .: "has_ceiling"
    _fixedTime  <- fmap (coerce @Int64) <$> t .:? "fixed_time"
    pure DimensionSettings{..}

instance ToNBT MonsterSpawnLightLevel where
  toNBT (LightLevel lvl) = toNBT lvl
  toNBT (Uniform ma mi) = compound [
      "type" .= T.pack "minecraft:uniform"
    , ("value", compound ["max_inclusive" .= ma, "min_inclusive" .= mi])
    ]

instance FromNBT MonsterSpawnLightLevel where
  parseNBT (TagInt lvl) = pure $ LightLevel lvl
  parseNBT c = withCompound (\t -> do
    t .: "type" >>= \case
      ("minecraft:uniform" :: Text) -> pure ()
      _ -> undefined -- TODO
    MinMax ma mi <- t .: "value"
    pure $ Uniform ma mi
    ) c

data MinMax = MinMax !Int32 !Int32

instance FromNBT MinMax where
  parseNBT = withCompound $ \t ->
    MinMax <$> t .: "max_inclusive" <*> t .: "min_inclusive"

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

instance FromNBT BiomeRegistry where
  parseNBT = withCompound $ \t ->
    BiomeRegistry <$> t .: "type" <*> t .: "value"

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

instance FromNBT BiomeRegistryEntry where
  parseNBT = withCompound $ \t ->
    BiomeRegistryEntry
      <$> t .: "name"
      <*> (fromIntegral @Int32 <$> t .: "id")
      <*> t .: "element"

instance ToNBT BiomeSettings where
  toNBT BiomeSettings{..} =
    compound $ [
        ("precipitation" .= _precipitation)
      , ("temperature" .= coerce @_ @Float _temperature)
      , ("downfall" .= coerce @_ @Float _downfall)
      , ("effects" .= _effects)
      -- , ("particle" .= _particle)
      ]
        <> (maybe [] (\x -> [x]) $ fmap (\el -> ("temperature_modifer" .= el)) _temperatureModifier)
        <> (maybe [] (\x -> [x]) $ fmap (\el -> ("category" .= el)) _category)

instance FromNBT BiomeSettings where
  parseNBT = withCompound $ \t -> do
    _precipitation <- t .: "precipitation"
    _temperature <- coerce @Float <$> t .: "temperature"
    _downfall <- coerce @Float <$> t .: "downfall"
    _effects <- t .: "effects"
    _temperatureModifier <- t .:? "temperature_modifer"
    _category <- t .:? "category"
    let _depth = Nothing
        _scale = Nothing 
    pure BiomeSettings{..}

instance ToNBT Precipitation where
  toNBT Rain = toNBT @Text "rain"
  toNBT Snow = toNBT @Text "snow"
  toNBT None = toNBT @Text "none"

instance FromNBT Precipitation where
  parseNBT (TagString "rain") = pure Rain
  parseNBT (TagString "snow") = pure Snow
  parseNBT (TagString "none") = pure None
  parseNBT _ = undefined -- TODO

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

instance FromNBT BiomeCategory where
  parseNBT (TagString "ocean") = pure OceanC
  parseNBT (TagString "plains") = pure PlainsC
  parseNBT (TagString "desert") = pure DesertC
  parseNBT (TagString "extreme_hills") = pure ExtremeHillsC
  parseNBT (TagString "taiga") = pure TaigaC
  parseNBT (TagString "swamp") = pure SwampC
  parseNBT (TagString "river") = pure RiverC
  parseNBT (TagString "nether") = pure NetherC
  parseNBT (TagString "the_end") = pure TheEndC
  parseNBT (TagString "icy") = pure IcyC
  parseNBT (TagString "mushroom") = pure MushroomC
  parseNBT (TagString "beach") = pure BeachC
  parseNBT (TagString "jungle") = pure JungleC
  parseNBT (TagString "mesa") = pure MesaC
  parseNBT (TagString "savanna") = pure SavannaC
  parseNBT (TagString "forest") = pure ForestC
  parseNBT (TagString "underground") = pure UndergroundC
  parseNBT (TagString "mountain") = pure MountainC
  parseNBT (TagString "none") = pure NoneC
  parseNBT _ = undefined -- TODO


instance ToNBT TemperatureModifier where
  toNBT Frozen = toNBT @Text "frozen"

instance FromNBT TemperatureModifier where
  parseNBT (TagString "frozen") = pure Frozen
  parseNBT _ = undefined -- TODO

instance ToNBT BiomeEffects where
  toNBT BiomeEffects{..} =
    compound $ [
        "sky_color" .= coerce @_ @Int32 _skyColor
      , "water_fog_color" .= coerce @_ @Int32 _waterFogColor
      , "fog_color" .= coerce @_ @Int32 _fogColor
      , "water_color" .= coerce @_ @Int32 _waterColor
      ]

instance FromNBT BiomeEffects where
  parseNBT = withCompound $ \t ->
    BiomeEffects
      <$> (coerce @Int32 <$> t .: "sky_color")
      <*> (coerce @Int32 <$> t .: "water_fog_color")
      <*> (coerce @Int32 <$> t .: "fog_color")
      <*> (coerce @Int32 <$> t .: "water_color")

data ChatRegistry = ChatRegistry {
  _chattype :: Text
, _chatvalue :: V.Vector ChatRegistryEntry
}
  deriving stock (Eq, Show)

instance ToNBT ChatRegistry where
  toNBT ChatRegistry{..} =
    compound [
        ("type" .= _chattype)
      , ("value" .= _chatvalue)
      ]

instance FromNBT ChatRegistry where
  parseNBT = withCompound $ \t ->
    ChatRegistry <$> t .: "type" <*> t .: "value"

data ChatRegistryEntry = ChatRegistryEntry {
  _chatname :: Text
, _chatid   :: Int
, _chatelement :: ChatSettings
}
  deriving stock (Eq, Show)

instance ToNBT ChatRegistryEntry where
  toNBT ChatRegistryEntry{..} =
    compound [
        "name" .= _chatname
      , "id" .= fromIntegral @_ @Int32 _chatid
      , "element" .= _chatelement
      ]

instance FromNBT ChatRegistryEntry where
  parseNBT = withCompound $ \t ->
    ChatRegistryEntry
      <$> t .: "name"
      <*> (fromIntegral @Int32 <$> t .: "id")
      <*> t .: "element"

-- TODO MOVE
data ChatSettings = ChatSettings
  deriving stock (Eq, Show)

instance ToNBT ChatSettings where
  toNBT ChatSettings{} = compound []

instance FromNBT ChatSettings where
  parseNBT = withCompound $ \_ -> pure ChatSettings
