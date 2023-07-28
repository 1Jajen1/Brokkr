{-# LANGUAGE OverloadedStrings #-}
module Brokkr.Registry.Dimension (
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
  ultrawarm = IsUltrawarm False
, monsterSpawnLightLevel = Uniform 7 0
, monsterSpawnBlockLightLimit = 0
, natural = IsNatural True
, coordinateScale = 1.0
, piglinSafe = IsPiglinSafe False
, respawnAnchorWorks = RespawnAnchorWorks False
, bedWorks = BedWorks True
, hasRaids = HasRaids True
, hasSkylight = HasSkylight True
, hasCeiling = HasCeiling False
, fixedTime = Nothing
, ambientLight = 0.0
, logicalHeight = 384
, infiniburn = "#minecraft:infiniburn_overworld"
, effects = "minecraft:overworld"
, height = 384
, minY = -64
}

nether :: DimensionSettings
nether = DimensionSettings {
  ultrawarm = IsUltrawarm True
, monsterSpawnLightLevel = LightLevel 11
, monsterSpawnBlockLightLimit = 15
, natural = IsNatural False
, coordinateScale = 8.0
, piglinSafe = IsPiglinSafe True
, respawnAnchorWorks = RespawnAnchorWorks True
, bedWorks = BedWorks False
, hasRaids = HasRaids False
, hasSkylight = HasSkylight False
, hasCeiling = HasCeiling True
, fixedTime = Just 18000
, ambientLight = 0.1
, logicalHeight = 128
, infiniburn = "#minecraft:infiniburn_nether"
, effects = "minecraft:the_nether"
, height = 256
, minY = 0
}

end :: DimensionSettings
end = DimensionSettings {
  ultrawarm = IsUltrawarm False
, monsterSpawnLightLevel = Uniform 7 0
, monsterSpawnBlockLightLimit = 0
, natural = IsNatural False
, coordinateScale = 1.0
, piglinSafe = IsPiglinSafe False
, respawnAnchorWorks = RespawnAnchorWorks False
, bedWorks = BedWorks False
, hasRaids = HasRaids True
, hasSkylight = HasSkylight False
, hasCeiling = HasCeiling False
, fixedTime = Just 6000
, ambientLight = 0.0
, logicalHeight = 256
, infiniburn = "#minecraft:infiniburn_end"
, effects = "minecraft:the_end"
, height = 256
, minY = 0
}

data DimensionSettings = DimensionSettings {
  piglinSafe         :: !IsPiglinSafe
, hasRaids           :: !HasRaids
, monsterSpawnLightLevel
                     :: !MonsterSpawnLightLevel
, monsterSpawnBlockLightLimit
                     :: !MonsterSpawnBlockLightLimit
, natural            :: !IsNatural
, ambientLight       :: !AmbientLight
, fixedTime          :: !(Maybe MCTime)
, infiniburn         :: !Infiniburn
, respawnAnchorWorks :: !RespawnAnchorWorks
, hasSkylight        :: !HasSkylight
, bedWorks           :: !BedWorks
, effects            :: !Effects
, minY               :: !MinY
, height             :: !Height
, logicalHeight      :: !LogicalHeight
, coordinateScale    :: !CoordinateScale
, ultrawarm          :: !IsUltrawarm
, hasCeiling         :: !HasCeiling
}
  deriving stock (Show, Eq)

newtype IsPiglinSafe = IsPiglinSafe Bool
  deriving stock Show
  deriving newtype Eq

newtype MonsterSpawnBlockLightLimit = MonsterSpawnBlockLightLimit Int32
  deriving stock Show
  deriving newtype (Eq, Ord, Enum, Integral, Real, Num)

-- Only valid if min > max
-- TODO Enforce this with a smart constructor and also name the fields!
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
