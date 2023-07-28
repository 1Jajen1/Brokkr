{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Brokkr.Packet.ServerToClient.Play.Types.Codec (
  RegistryCodec(..)
, DimensionName(..), mkDimensionName
, DimensionRegistry(..), mkDimensionRegistry
, DimensionRegistryEntry(..)
, BiomeRegistry(..), mkBiomeRegistry
, BiomeName(..), mkBiomeName
, BiomeRegistryEntry(..)
, ChatRegistry(..), mkChatRegistry
, ChatName(..), mkChatName
, ChatRegistryEntry(..)
) where


import Brokkr.NBT qualified as NBT
import Brokkr.NBT.Internal qualified as NBT
import Brokkr.NBT.Slice qualified as NBT.Slice
import Brokkr.NBT.Codec

import Brokkr.Registry.BiomeSettings as Biome
import Brokkr.Registry.Dimension as Dimension

import Brokkr.Packet.Binary
import Brokkr.Packet.Common.Internal

import Data.Coerce
import Data.Int
import Data.Text

import Data.Vector qualified as V

data RegistryCodec = RegistryCodec {
  codecDimRegistry :: !DimensionRegistry
, codecBiomes      :: !BiomeRegistry
, codecChat        :: !ChatRegistry
}
  deriving stock (Eq, Show)

instance HasCodec RegistryCodec where
  codec = compound "RegistryCodec" $ [|| RegistryCodec ||]
    <$#> requiredField "minecraft:dimension_type" .= [|| codecDimRegistry ||]
    <*#> requiredField "minecraft:worldgen/biome" .= [|| codecBiomes ||]
    <*#> requiredField "minecraft:chat_type" .= [|| codecChat ||]

-- Unsafe because the dimId in the entry has to be the index into this vector
-- mkDimensionRegistry ensures that
newtype DimensionRegistry = UnsafeDimensionRegistry { unDimensionRegistry :: V.Vector DimensionRegistryEntry }
  deriving stock Show
  deriving newtype Eq

instance HasCodec DimensionRegistry where
  codec = compound "DimensionRegistry" $ [|| \_ els -> UnsafeDimensionRegistry els ||]
    <$#> requiredField "type"  .= [|| \_ -> NBT.fromText "minecraft:dimension_type" ||]
    <*#> requiredField "value" .= [|| coerce ||]

mkDimensionRegistry :: V.Vector (DimensionName, DimensionSettings) -> DimensionRegistry
mkDimensionRegistry v = UnsafeDimensionRegistry $ V.generate (V.length v) $ \i ->
  let (n,s) = V.unsafeIndex v i
  in DimensionRegistryEntry n (fromIntegral i) s

newtype DimensionName = UnsafeDimensionName Text
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via Identifier

mkDimensionName :: Text -> Maybe DimensionName
mkDimensionName = coerce . mkIdentifier

data DimensionRegistryEntry = DimensionRegistryEntry {
  dimName     :: !DimensionName
, dimId       :: !Int32
, dimSettings :: !DimensionSettings
}
  deriving stock (Eq, Show)

instance HasCodec DimensionRegistryEntry where
  codec = compound "DimensionRegistryEntry" $ [|| DimensionRegistryEntry ||]
    <$#> requiredFieldVia @Identifier "name" .= [|| dimName ||]
    <*#> requiredField "id" .= [|| dimId ||]
    <*#> requiredField "element" .= [|| dimSettings ||]

instance HasCodec DimensionSettings where
  codec = compound "DimensionSettings" $ [|| DimensionSettings ||]
    <$#> requiredFieldVia @Bool "piglin_safe" .= [|| piglinSafe ||]
    <*#> requiredFieldVia @Bool "has_raids" .= [|| hasRaids ||]
    <*#> monsterSpawnLightLevelCodec
    <*#> requiredFieldVia @Int32 "monster_spawn_block_light_limit" .= [|| monsterSpawnBlockLightLimit ||]
    <*#> requiredFieldVia @Bool "natural" .= [|| natural ||]
    <*#> requiredFieldVia @Float "ambient_light" .= [|| ambientLight ||]
    <*#> optionalFieldVia @Int64 "fixed_time" .= [|| fixedTime ||]
    <*#> requiredFieldVia @Identifier "infiniburn" .= [|| infiniburn ||]
    <*#> requiredFieldVia @Bool "respawn_anchor_works" .= [|| respawnAnchorWorks ||]
    <*#> requiredFieldVia @Bool "has_skylight" .= [|| hasSkylight ||]
    <*#> requiredFieldVia @Bool "bed_works" .= [|| bedWorks ||]
    <*#> requiredFieldVia @Text "effects" .= [|| Dimension.effects ||]
    <*#> requiredFieldVia @Int32 "min_y" .= [|| minY ||]
    <*#> requiredFieldVia @Int32 "height" .= [|| height ||]
    <*#> requiredFieldVia @Int32 "logical_height" .= [|| logicalHeight ||]
    <*#> requiredFieldVia @Double "coordinate_scale" .= [|| coordinateScale ||]
    <*#> requiredFieldVia @Bool "ultrawarm" .= [|| ultrawarm ||]
    <*#> requiredFieldVia @Bool "has_ceiling" .= [|| hasCeiling ||]
    where
      monsterSpawnLightLevelCodec = rmapEitherCodec
        [|| \case
          NBT.TagInt i -> Right $ LightLevel i
          -- TODO All of the decoding side here is broken. This decodes min/max directly, but instead it should
          -- check the type, then read the value compound which contains min/max
          NBT.TagCompound c -> case NBT.Slice.findWithIndex (\(NBT.NBT k _) -> k) "min_inclusive" c of
            (# _, (# NBT.NBT _ (NBT.TagInt mi) | #) #) -> case NBT.Slice.findWithIndex (\(NBT.NBT k _) -> k) "max_inclusive" c of
              (# _, (# NBT.NBT _ (NBT.TagInt ma) | #) #) -> Right $ Uniform mi ma
              (# _, (# | _ #) #) -> Left $ NBT.MissingKey "MonsterLightLevel.max_inclusive"
            (# _, (# | _ #) #) -> Left $ NBT.MissingKey "MonsterLightLevel.min_inclusive"
          ||] $ requiredField "monster_spawn_light_level" .= [|| \d -> case monsterSpawnLightLevel d of
            LightLevel i -> NBT.TagInt i
            Uniform ma mi -> NBT.TagCompound $ NBT.Slice.fromList [
                NBT.NBT "type" (NBT.TagString "minecraft:uniform")
              , NBT.NBT "value" $ NBT.TagCompound $ NBT.Slice.fromList [NBT.NBT "min_inclusive" (NBT.TagInt mi), NBT.NBT "max_inclusive" (NBT.TagInt ma)]
              ]
          ||]

-- Unsafe because the dimId in the entry has to be the index into this vector
-- mkDimensionRegistry ensures that
newtype BiomeRegistry = UnsafeBiomeRegistry { unBiomeRegistry :: V.Vector BiomeRegistryEntry }
  deriving stock Show
  deriving newtype Eq

instance HasCodec BiomeRegistry where
  codec = compound "BiomeRegistry" $ [|| \_ els -> UnsafeBiomeRegistry els ||]
    <$#> requiredField "type"  .= [|| \_ -> NBT.fromText "minecraft:worldgen/biome" ||]
    <*#> requiredField "value" .= [|| coerce ||]

mkBiomeRegistry :: V.Vector (BiomeName, BiomeSettings) -> BiomeRegistry
mkBiomeRegistry v = UnsafeBiomeRegistry $ V.generate (V.length v) $ \i ->
  let (n,s) = V.unsafeIndex v i
  in BiomeRegistryEntry n (fromIntegral i) s

newtype BiomeName = UnsafeBiomeName { unBiomeName :: Text }
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via Identifier

mkBiomeName :: Text -> Maybe BiomeName
mkBiomeName = coerce . mkIdentifier

data BiomeRegistryEntry = BiomeRegistryEntry {
  biomeName     :: !BiomeName
, biomeId       :: !Int32
, biomeSettings :: !BiomeSettings
}
  deriving stock (Eq, Show)

instance HasCodec BiomeRegistryEntry where
  codec = compound "BiomeRegistryEntry" $ [|| BiomeRegistryEntry ||]
    <$#> requiredFieldVia @Identifier "name" .= [|| biomeName ||]
    <*#> requiredField "id" .= [|| biomeId ||]
    <*#> requiredField "element" .= [|| biomeSettings ||]

instance HasCodec BiomeSettings where
  codec = compound "BiomeSettings" $ [|| BiomeSettings ||]
    <$#> requiredField "precipitation" .= [|| precipitation ||]
    <*#> optionalFieldVia @Float "depth" .= [|| depth ||]
    <*#> requiredFieldVia @Float "temperature" .= [|| temperature ||]
    <*#> optionalFieldVia @Float "scale" .= [|| scale ||]
    <*#> requiredFieldVia @Float "downfall" .= [|| downfall ||]
    <*#> optionalField "category" .= [|| category ||]
    <*#> optionalField "temperatureModifier" .= [|| temperatureModifier ||]
    <*#> requiredField "effects" .= [|| Biome.effects ||]

instance HasCodec Precipitation where
  codec = lmapCodec [|| \case
      PrecipitationSnow -> "snow"
      PrecipitationRain -> "rain"
      PrecipitationNone -> "none"
    ||] $ rmapEitherCodec [|| \case
      "snow" -> Right PrecipitationSnow
      "rain" -> Right PrecipitationRain
      "none" -> Right PrecipitationNone
      str    -> Left $ NBT.InvalidStringEnum (NBT.toText str) ["snow", "rain", "none"]
    ||] codec

instance HasCodec BiomeCategory where
  codec = lmapCodec [|| \case
      CategoryOcean -> "ocean"
      CategoryPlains -> "plains"
      CategoryDesert -> "desert"
      CategoryForest -> "forest"
      CategoryExtremeHills -> "extreme_hills"
      CategoryTaiga -> "taiga"
      CategorySwamp -> "swamp"
      CategoryRiver -> "river"
      CategoryNether -> "nether"
      CategoryTheEnd -> "the_end"
      CategoryIcy -> "icy"
      CategoryMushroom -> "mushroom"
      CategoryBeach -> "beach"
      CategoryJungle -> "jungle"
      CategoryMesa -> "mesa"
      CategorySavanna -> "savanna"
      CategoryUnderground -> "underground"
      CategoryMountain -> "mountain"
      CategoryNone -> "none"
    ||] $ rmapEitherCodec [|| \case
      "ocean" -> pure CategoryOcean
      "plains" -> pure CategoryPlains
      "desert" -> pure CategoryDesert
      "forest" -> pure CategoryForest
      "extreme_hills" -> pure CategoryExtremeHills
      "taiga" -> pure CategoryTaiga
      "swamp" -> pure CategorySwamp
      "river" -> pure CategoryRiver
      "nether" -> pure CategoryNether
      "the_end" -> pure CategoryTheEnd
      "icy" -> pure CategoryIcy
      "mushroom" -> pure CategoryMushroom
      "beach" -> pure CategoryBeach
      "jungle" -> pure CategoryJungle
      "mesa" -> pure CategoryMesa
      "savanna" -> pure CategorySavanna
      "underground" -> pure CategoryUnderground
      "mountain" -> pure CategoryMountain
      "none" -> pure CategoryNone
      str -> Left $ NBT.InvalidStringEnum (NBT.toText str)
        [ "ocean", "plains", "desert", "forest", "extreme_hills"
        , "taiga", "swamp", "river", "nether", "the_end", "icy"
        , "mushroom", "beach", "jungle", "mesa", "savanna"
        , "underground", "mountain", "none"
        ]
    ||] codec

instance HasCodec TemperatureModifier where
  codec = lmapCodec [|| \case
      Frozen -> "frozen"
    ||] $ rmapEitherCodec [|| \case
      "frozen" -> pure Frozen
      str -> Left $ NBT.InvalidStringEnum (NBT.toText str) ["frozen"]
    ||] codec

instance HasCodec BiomeEffects where
  codec = compound "BiomeEffects" $ [|| BiomeEffects ||]
    <$#> requiredFieldVia @Int32 "sky_color" .= [|| skyColor ||]
    <*#> requiredFieldVia @Int32 "water_fog_color" .= [|| waterFogColor ||]
    <*#> requiredFieldVia @Int32 "fog_color" .= [|| fogColor ||]
    <*#> requiredFieldVia @Int32 "water_color" .= [|| waterColor ||]

newtype ChatRegistry = UnsafeChatRegistry { unChatRegistry :: V.Vector ChatRegistryEntry }
  deriving stock Show
  deriving newtype Eq

instance HasCodec ChatRegistry where
  codec = compound "ChatRegistry" $ [|| \_ els -> UnsafeChatRegistry els ||]
    <$#> requiredField "type"  .= [|| \_ -> NBT.fromText "minecraft:chat_type" ||]
    <*#> requiredField "value" .= [|| coerce ||]

mkChatRegistry :: V.Vector ChatRegistryEntry -> ChatRegistry -- TODO
mkChatRegistry = UnsafeChatRegistry

newtype ChatName = UnsafeChatName { unChatName :: Text }
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via Identifier

mkChatName :: Text -> Maybe ChatName
mkChatName = coerce . mkIdentifier

data ChatRegistryEntry = ChatRegistryEntry {
  chatName     :: !ChatName
, chatId       :: !Int32
, chatSettings :: !ChatSettings
}
  deriving stock (Eq, Show)

instance HasCodec ChatRegistryEntry where
  codec = compound "ChatRegistryEntry" $ [|| ChatRegistryEntry ||]
    <$#> requiredFieldVia @Identifier "name" .= [|| chatName ||]
    <*#> requiredField "id" .= [|| chatId ||]
    <*#> requiredField "element" .= [|| chatSettings ||]

data ChatSettings = ChatSettings
  deriving stock (Eq, Show)

instance HasCodec ChatSettings where
  codec = compound "ChatSettings" $ pureC [|| ChatSettings ||]
