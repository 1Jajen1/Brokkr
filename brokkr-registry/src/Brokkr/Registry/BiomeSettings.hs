{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Brokkr.Registry.BiomeSettings (
  BiomeSettings(..)
, Precipitation(..)
, Depth(..)
, Temperature(..)
, Scale(..)
, Downfall(..)
, BiomeCategory(..)
, TemperatureModifier(..)
, BiomeEffects(..)
, SkyColor(..)
, WaterFogColor(..)
, FogColor(..)
, WaterColor(..)
, FoliageColor(..)
, GrassColor(..)
, GrassColorModifier(..)
) where

import Data.Aeson
import Deriving.Aeson qualified as A

import Data.Int

import Data.Text qualified as T

import GHC.Generics

import Language.Haskell.TH.Syntax (Lift)

data BiomeSettings = BiomeSettings {
  precipitation       :: !Precipitation
, depth               :: !(Maybe Depth)
, temperature         :: !Temperature
, scale               :: !(Maybe Scale)
, downfall            :: !Downfall
, category            :: !(Maybe BiomeCategory)
, temperatureModifier :: !(Maybe TemperatureModifier)
, effects             :: !BiomeEffects
}
  deriving stock (Show, Eq, Generic, Lift)
  deriving FromJSON via A.CustomJSON '[
    A.FieldLabelModifier A.CamelToSnake
  ] BiomeSettings

data Precipitation = PrecipitationSnow | PrecipitationRain | PrecipitationNone
  deriving stock (Show, Eq, Lift)

instance FromJSON Precipitation where
  parseJSON = withText "precipitation" $ \case
    "snow" -> pure PrecipitationSnow
    "rain" -> pure PrecipitationRain
    "none" -> pure PrecipitationNone
    t -> fail $ "Unknown value " <> T.unpack t

newtype Depth = Depth Float
  deriving stock (Show, Lift)
  deriving newtype (Eq, FromJSON)

newtype Temperature = Temperature Float
  deriving stock (Show, Lift)
  deriving newtype (Eq, FromJSON)

newtype Scale = Scale Float
  deriving stock (Show, Lift)
  deriving newtype (Eq, FromJSON)

newtype Downfall = Downfall Float
  deriving stock (Show, Lift)
  deriving newtype (Eq, FromJSON)

data BiomeCategory =
    CategoryOcean
  | CategoryPlains
  | CategoryDesert
  | CategoryForest
  | CategoryExtremeHills
  | CategoryTaiga
  | CategorySwamp
  | CategoryRiver
  | CategoryNether
  | CategoryTheEnd
  | CategoryIcy
  | CategoryMushroom
  | CategoryBeach
  | CategoryJungle
  | CategoryMesa
  | CategorySavanna
  | CategoryUnderground
  | CategoryMountain
  | CategoryNone
  deriving stock (Show, Eq, Lift)

instance FromJSON BiomeCategory where
  parseJSON = withText "category" $ \case
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
    t -> fail $ "Unknown value " <> T.unpack t

data TemperatureModifier = Frozen
  deriving stock (Show, Eq, Lift)

instance FromJSON TemperatureModifier where
  parseJSON = withText "temperature_modifier" $ \case
    "frozen" -> pure Frozen
    _ -> fail "unknown value for temperature_modifier"

data BiomeEffects = BiomeEffects {
  skyColor      :: !SkyColor
, waterFogColor :: !WaterFogColor
, fogColor      :: !FogColor
, waterColor    :: !WaterColor
-- TODO Remaining options
}
  deriving stock (Show, Eq, Generic, Lift)
  deriving FromJSON via A.CustomJSON '[
    A.FieldLabelModifier A.CamelToSnake
  ] BiomeEffects

newtype SkyColor = SkyColor Int32
  deriving stock (Show, Lift)
  deriving newtype (Eq, FromJSON)

newtype WaterFogColor = WaterFogColor Int32
  deriving stock (Show, Lift)
  deriving newtype (Eq, FromJSON)

newtype FogColor = FogColor Int32
  deriving stock (Show, Lift)
  deriving newtype (Eq, FromJSON)

newtype WaterColor = WaterColor Int32
  deriving stock (Show, Lift)
  deriving newtype (Eq, FromJSON)

newtype FoliageColor = FoliageColor Int32
  deriving stock (Show, Lift)
  deriving newtype (Eq, FromJSON)

newtype GrassColor = GrassColor Int32
  deriving stock (Show, Lift)
  deriving newtype (Eq, FromJSON)

data GrassColorModifier = ModifierSwamp | ModifierDarkForest
  deriving stock (Show, Eq, Lift)

instance FromJSON GrassColorModifier where
  parseJSON = withText "grass_color_modifier" $ \case
    "swamp" -> pure ModifierSwamp
    "dark_forest" -> pure ModifierDarkForest
    _ -> fail "unknown value for grass_color_modifier"
