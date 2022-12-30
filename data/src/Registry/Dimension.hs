{-# LANGUAGE OverloadedStrings #-}
module Registry.Dimension (
  DimensionSettings(..)
, IsPiglinSafe(..)
, IsNatural(..)
, AmbientLight(..)
, MCTime(..)
, Infiniburn(..)
, RespawnAnchorWorks(..)
, HasSkylight(..)
, BedWorks(..)
, Effects(..)
, HasRaids(..)
, MinY(..)
, Height(..)
, LogicalHeight(..)
, CoordinateScale(..)
, IsUltrawarm(..)
, HasCeiling(..)
, MonsterSpawnLightLevel(..)
, MonsterSpawnBlockLightLimit(..)
, overworld, nether, end
) where

import Data.Int
import Data.String
import Data.Text

overworld :: DimensionSettings
overworld = DimensionSettings {
  _ultrawarm = IsUltrawarm False
, _monster_spawn_light_level = Uniform 7 0
, _monster_spawn_block_light_limit = 0
, _natural = IsNatural True
, _coordinateScale = 1.0
, _piglinSafe = IsPiglinSafe False
, _respawnAnchorWorks = RespawnAnchorWorks False
, _bedWorks = BedWorks True
, _hasRaids = HasRaids True
, _hasSkylight = HasSkylight True
, _hasCeiling = HasCeiling False
, _fixedTime = Nothing
, _ambientLight = 0.0
, _logicalHeight = 384
, _infiniburn = "#minecraft:infiniburn_overworld"
, _effects = "minecraft:overworld"
, _height = 384
, _minY = -64
}

nether :: DimensionSettings
nether = DimensionSettings {
  _ultrawarm = IsUltrawarm True
, _monster_spawn_light_level = LightLevel 11
, _monster_spawn_block_light_limit = 15
, _natural = IsNatural False
, _coordinateScale = 8.0
, _piglinSafe = IsPiglinSafe True
, _respawnAnchorWorks = RespawnAnchorWorks True
, _bedWorks = BedWorks False
, _hasRaids = HasRaids False
, _hasSkylight = HasSkylight False
, _hasCeiling = HasCeiling True
, _fixedTime = Just 18000
, _ambientLight = 0.1
, _logicalHeight = 128
, _infiniburn = "#minecraft:infiniburn_nether"
, _effects = "minecraft:the_nether"
, _height = 256
, _minY = 0
}

end :: DimensionSettings
end = DimensionSettings {
  _ultrawarm = IsUltrawarm False
, _monster_spawn_light_level = Uniform 7 0
, _monster_spawn_block_light_limit = 0
, _natural = IsNatural False
, _coordinateScale = 1.0
, _piglinSafe = IsPiglinSafe False
, _respawnAnchorWorks = RespawnAnchorWorks False
, _bedWorks = BedWorks False
, _hasRaids = HasRaids True
, _hasSkylight = HasSkylight False
, _hasCeiling = HasCeiling False
, _fixedTime = Just 6000
, _ambientLight = 0.0
, _logicalHeight = 256
, _infiniburn = "#minecraft:infiniburn_end"
, _effects = "minecraft:the_end"
, _height = 256
, _minY = 0
}

data DimensionSettings = DimensionSettings {
  _piglinSafe         :: IsPiglinSafe
, _hasRaids           :: HasRaids
, _monster_spawn_light_level
                      :: MonsterSpawnLightLevel
, _monster_spawn_block_light_limit
                      :: MonsterSpawnBlockLightLimit
, _natural            :: IsNatural
, _ambientLight       :: AmbientLight
, _fixedTime          :: Maybe MCTime
, _infiniburn         :: Infiniburn
, _respawnAnchorWorks :: RespawnAnchorWorks
, _hasSkylight        :: HasSkylight
, _bedWorks           :: BedWorks
, _effects            :: Effects
, _minY               :: MinY
, _height             :: Height
, _logicalHeight      :: LogicalHeight
, _coordinateScale    :: CoordinateScale
, _ultrawarm          :: IsUltrawarm
, _hasCeiling         :: HasCeiling
}
  deriving stock (Show, Eq)

newtype IsPiglinSafe = IsPiglinSafe Bool
  deriving stock Show
  deriving newtype Eq

newtype MonsterSpawnBlockLightLimit = MonsterSpawnBlockLightLimit Int32
  deriving stock Show
  deriving newtype (Eq, Ord, Enum, Integral, Real, Num)

data MonsterSpawnLightLevel = LightLevel Int32 | Uniform Int32 Int32
  deriving stock (Show, Eq)

newtype IsNatural = IsNatural Bool
  deriving stock Show
  deriving newtype Eq

newtype AmbientLight = AmbientLight Float
  deriving stock Show
  deriving newtype (Eq, Ord, Fractional, Real, Num)

newtype MCTime = MCTime Int64
  deriving stock Show
  deriving newtype (Eq, Ord, Enum, Integral, Real, Num)

newtype Infiniburn = Infiniburn Text
  deriving stock Show
  deriving newtype (Eq, IsString)

newtype RespawnAnchorWorks = RespawnAnchorWorks Bool
  deriving stock Show
  deriving newtype Eq

newtype HasSkylight = HasSkylight Bool
  deriving stock Show
  deriving newtype Eq

newtype BedWorks = BedWorks Bool
  deriving stock Show
  deriving newtype Eq

newtype Effects = Effects Text
  deriving stock Show
  deriving newtype (Eq, IsString)

newtype HasRaids = HasRaids Bool
  deriving stock Show
  deriving newtype Eq

newtype LogicalHeight = LogicalHeight Int32
  deriving stock Show
  deriving newtype (Eq, Ord, Enum, Integral, Real, Num)

newtype CoordinateScale = CoordinateScale Double -- TODO Double or Float
  deriving stock Show
  deriving newtype (Eq, Ord, Fractional, Real, Num)

newtype IsUltrawarm = IsUltrawarm Bool
  deriving stock Show
  deriving newtype Eq

newtype HasCeiling = HasCeiling Bool
  deriving stock Show
  deriving newtype Eq

newtype MinY = MinY Int32
  deriving stock Show
  deriving newtype (Eq, Ord, Real, Num)

newtype Height = Height Int32
  deriving stock Show
  deriving newtype (Eq, Ord, Real, Num)
