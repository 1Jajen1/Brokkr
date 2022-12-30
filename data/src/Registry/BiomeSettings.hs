{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Registry.BiomeSettings (
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
import Data.Int
import qualified Data.Text as T
import qualified Deriving.Aeson as A
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)

data BiomeSettings = BiomeSettings {
  _precipitation       :: Precipitation
, _depth               :: Maybe Depth
, _temperature         :: Temperature
, _scale               :: Maybe Scale
, _downfall            :: Downfall
, _category            :: Maybe BiomeCategory
, _temperatureModifier :: Maybe TemperatureModifier
, _effects             :: BiomeEffects
-- TODO Remaining options
}
  deriving stock (Show, Eq, Generic, Lift)
  deriving FromJSON via A.CustomJSON '[
    A.FieldLabelModifier (A.CamelToSnake, A.StripPrefix "_")
  ] BiomeSettings

data Precipitation = Snow | Rain | None
  deriving stock (Show, Eq, Lift)

instance FromJSON Precipitation where
  parseJSON = withText "precipitation" $ \case
    "snow" -> pure Snow
    "rain" -> pure Rain
    "none" -> pure None
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
    OceanC
  | PlainsC
  | DesertC
  | ForestC
  | ExtremeHillsC
  | TaigaC
  | SwampC
  | RiverC
  | NetherC
  | TheEndC
  | IcyC
  | MushroomC
  | BeachC
  | JungleC
  | MesaC
  | SavannaC
  | UndergroundC
  | MountainC
  | NoneC
  deriving stock (Show, Eq, Lift)

instance FromJSON BiomeCategory where
  parseJSON = withText "category" $ \case
    "ocean" -> pure OceanC
    "plains" -> pure PlainsC
    "desert" -> pure DesertC
    "forest" -> pure ForestC
    "extreme_hills" -> pure ExtremeHillsC
    "taiga" -> pure TaigaC
    "swamp" -> pure SwampC
    "river" -> pure RiverC
    "nether" -> pure NetherC
    "the_end" -> pure TheEndC
    "icy" -> pure IcyC
    "mushroom" -> pure MushroomC
    "beach" -> pure BeachC
    "jungle" -> pure JungleC
    "mesa" -> pure MesaC
    "savanna" -> pure SavannaC
    "underground" -> pure UndergroundC
    "mountain" -> pure MountainC
    "none" -> pure NoneC
    t -> fail $ "Unknown value " <> T.unpack t

data TemperatureModifier = Frozen
  deriving stock (Show, Eq, Lift)

instance FromJSON TemperatureModifier where
  parseJSON = withText "temperature_modifier" $ \case
    "frozen" -> pure Frozen
    _ -> fail "unknown value for temperature_modifier"

data BiomeEffects = BiomeEffects {
  _skyColor           :: SkyColor
, _waterFogColor      :: WaterFogColor
, _fogColor           :: FogColor
, _waterColor         :: WaterColor
-- TODO Remaining options
}
  deriving stock (Show, Eq, Generic, Lift)
  deriving FromJSON via A.CustomJSON '[
    A.FieldLabelModifier (A.CamelToSnake, A.StripPrefix "_")
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

data GrassColorModifier = SwampM | DarkForestM
  deriving stock (Show, Eq, Lift)

instance FromJSON GrassColorModifier where
  parseJSON = withText "grass_color_modifier" $ \case
    "swamp" -> pure SwampM
    "dark_forest" -> pure DarkForestM
    _ -> fail "unknown value for grass_color_modifier"
