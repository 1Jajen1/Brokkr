{-# LANGUAGE DataKinds, TemplateHaskellQuotes, DerivingStrategies, DeriveAnyClass, OverloadedStrings #-}
module Player where

import Brokkr.NBT
import Brokkr.NBT.Codec

import Control.DeepSeq

import Data.Int

import Data.Primitive
import Data.Vector.Storable qualified as S

import GHC.Generics (Generic)

data Player = Player {
  air :: Int16
, fallDistance :: Float
, fire :: Int16
-- , glowing :: Bool
-- , hasVisualFire :: Bool
, invulnerable :: Bool
, motion :: S.Vector (BigEndian Double)
-- , noGravity :: Bool
, onGround :: Bool
-- , passengers :: SmallArray Tag -- TODO
, portalCooldown :: Int32
, pos :: S.Vector (BigEndian Double)
, rotation :: S.Vector (BigEndian Float)
, silent :: Maybe Bool
-- , tags :: SmallArray Tag -- TODO
-- , ticksFrozen :: Int32
-- , uuid :: S.Vector Int32
, absorptionAmount :: Float
, activeEffects :: Maybe (SmallArray PotionEffect)
, attributes :: SmallArray Attribute
, deathTime :: Int16
-- , fallFlying :: Bool
, health :: Int16
, hurtByTimestamp :: Int32
, hurtTime :: Int16
-- , leftHanded :: Bool
, abilities :: PlayerAbilities
-- , dataVersion :: Int32
, dimension :: Int32
, enderItems :: SmallArray Tag -- TODO
-- , enteredNetherPosition :: Maybe Tag -- TODO
, foodExhaustionLevel :: Float
, foodLevel :: Int32
, foodSaturationLevel :: Float
, foodTickTimer :: Int32
, inventory :: SmallArray Slot
-- , lastDeathLocation :: Maybe Tag -- TODO
, playerGameType :: Int32
-- , previousPlayerGameType :: Int32
-- , recipeBook :: Tag -- TODO
, score :: Int32
, xpP :: Float
, healF :: Float
, spawnX :: Int32
, spawnY :: Int32
, spawnZ :: Int32
, xpSeed :: Int32
, xpLevel :: Int32
, xpTotal :: Int32
, sleeping :: Bool
, uuidMost :: Int64
, uuidLeast :: Int64
, selectedItem :: SelectedSlot
, selectedItemSlot :: Int32
}
  deriving stock Generic
  deriving anyclass NFData

data Slot = Slot {
  slotCount :: Int8
, slotSlot :: Int8
, slotId :: NBTString
, slotTag :: Maybe Tag
}
  deriving stock Generic
  deriving anyclass NFData

instance HasCodec SelectedSlot where
  codec = compound "SelectedSlot" $ [|| SelectedSlot ||]
    <$#> requiredField "Count" .= [|| selSlotCount ||]
    <*#> requiredField "id" .= [|| selSlotId ||]
    <*#> optionalField "tag" .= [|| selSlotTag ||]

data SelectedSlot = SelectedSlot {
  selSlotCount :: Int8
, selSlotId :: NBTString
, selSlotTag :: Maybe Tag
}
  deriving stock Generic
  deriving anyclass NFData

instance HasCodec Slot where
  codec = compound "Slot" $ [|| Slot ||]
    <$#> requiredField "Count" .= [|| slotCount ||]
    <*#> requiredField "Slot" .= [|| slotSlot ||]
    <*#> requiredField "id" .= [|| slotId ||]
    <*#> optionalField "tag" .= [|| slotTag ||]

data PlayerAbilities = PlayerAbilities {
  paFlying :: Bool
, paFlySpeed :: Float
, paInstabuild :: Bool
, paInvulnerable :: Bool
, paMayBuild :: Bool
, paMayFly :: Bool
, paWalkSpeed :: Float
}
  deriving stock Generic
  deriving anyclass NFData

instance HasCodec PlayerAbilities where
  codec = compound "PlayerAbilities" $ [|| PlayerAbilities ||]
    <$#> requiredField "flying" .= [|| paFlying ||]
    <*#> requiredField "flySpeed" .= [|| paFlySpeed ||]
    <*#> requiredField "instabuild" .= [|| paInstabuild ||]
    <*#> requiredField "invulnerable" .= [|| paInvulnerable ||]
    <*#> requiredField "mayBuild" .= [|| paMayBuild ||]
    <*#> requiredField "mayfly" .= [|| paMayFly ||]
    <*#> requiredField "walkSpeed" .= [|| paWalkSpeed ||]

data Attribute = Attribute {
  aBase :: Double
, aModifiers :: Maybe (SmallArray Modifier)
, aName :: NBTString
}
  deriving stock Generic
  deriving anyclass NFData

instance HasCodec Attribute where
  codec = compound "Attribute" $ [|| Attribute ||]
    <$#> requiredField "Base" .= [|| aBase ||]
    <*#> optionalField "Modifiers" .= [|| aModifiers ||]
    <*#> requiredField "Name" .= [|| aName ||]

data Modifier = Modifier {
  mAmount :: Double
, mName :: NBTString
, mOperation :: Int32
, mUUID :: Maybe (S.Vector Int32)
}
  deriving stock Generic
  deriving anyclass NFData

instance HasCodec Modifier where
  codec = compound "Modifier" $ [|| Modifier ||]
    <$#> requiredField "Amount" .= [|| mAmount ||]
    <*#> requiredField "Name" .= [|| mName ||]
    <*#> requiredField "Operation" .= [|| mOperation ||]
    <*#> optionalField "UUID" .= [|| mUUID ||]

data PotionEffect = PotionEffect {
  peAmbient :: Bool
, peAmplifier :: Int8
, peDuration :: Int32
-- , peHiddenEffect :: Maybe PotionEffect
, peId :: Int8
, peShowParticles :: Bool
}
  deriving stock Generic
  deriving anyclass NFData

instance HasCodec PotionEffect where
  codec = compound "PotionEffect" $ [|| PotionEffect ||]
    <$#> requiredField "Ambient" .= [|| peAmbient ||]
    <*#> requiredField "Amplifier" .= [|| peAmplifier ||]
    <*#> requiredField "Duration" .= [|| peDuration ||]
    -- <*#> optionalField "HiddenEffect" .= [|| peHiddenEffect ||]
    <*#> requiredField "Id" .= [|| peId ||]
    <*#> requiredField "ShowParticles" .= [|| peShowParticles ||]

playerCodec :: NBTCodec Value Player Player
playerCodec = compound "Player" $ [|| Player ||]
  <$#> requiredField "Air" .= [|| air ||]
  <*#> requiredField "FallDistance" .= [|| fallDistance ||]
  <*#> requiredField "Fire" .= [|| fire ||]
  -- <*#> requiredField "Glowing" .= [|| glowing ||]
  -- <*#> requiredField "HasVisualFire" .= [|| hasVisualFire ||]
  <*#> requiredField "Invulnerable" .= [|| invulnerable ||]
  <*#> requiredFieldVia @(ViaList (S.Vector (BigEndian Double))) "Motion" .= [|| motion ||]
  -- <*#> requiredField "NoGravity" .= [|| noGravity ||]
  <*#> requiredField "OnGround" .= [|| onGround ||]
  -- <*#> requiredField "Passengers" .= [|| passengers ||]
  <*#> requiredField "PortalCooldown" .= [|| portalCooldown ||]
  <*#> requiredFieldVia @(ViaList (S.Vector (BigEndian Double))) "Pos" .= [|| pos ||]
  <*#> requiredFieldVia @(ViaList (S.Vector (BigEndian Float))) "Rotation" .= [|| rotation ||]
  <*#> optionalField "Silent" .= [|| silent ||]
  -- <*#> requiredField "Tags" .= [|| tags ||]
  -- <*#> requiredField "TicksFrozen" .= [|| ticksFrozen ||]
  -- <*#> requiredField "UUID" .= [|| uuid ||]
  <*#> requiredField "AbsorptionAmount" .= [|| absorptionAmount ||]
  <*#> optionalField "ActiveEffects" .= [|| activeEffects ||]
  <*#> requiredField "Attributes" .= [|| attributes ||]
  <*#> requiredField "DeathTime" .= [|| deathTime ||]
  -- <*#> requiredField "FallFlying" .= [|| fallFlying ||]
  <*#> requiredField "Health" .= [|| health ||]
  <*#> requiredField "HurtByTimestamp" .= [|| hurtByTimestamp ||]
  <*#> requiredField "HurtTime" .= [|| hurtTime ||]
  -- <*#> requiredField "LeftHanded" .= [|| leftHanded ||]
  <*#> requiredField "abilities" .= [|| abilities ||]
  -- <*#> requiredField "DataVersion" .= [|| dataVersion ||]
  <*#> requiredField "Dimension" .= [|| dimension ||]
  <*#> requiredField "EnderItems" .= [|| enderItems ||]
  -- <*#> optionalField "enteredNetherPosition" .= [|| enteredNetherPosition ||]
  <*#> requiredField "foodExhaustionLevel" .= [|| foodExhaustionLevel ||]
  <*#> requiredField "foodLevel" .= [|| foodLevel ||]
  <*#> requiredField "foodSaturationLevel" .= [|| foodSaturationLevel ||]
  <*#> requiredField "foodTickTimer" .= [|| foodTickTimer ||]
  <*#> requiredField "Inventory" .= [|| inventory ||]
  -- <*#> optionalField "LastDeathLocation" .= [|| lastDeathLocation ||]
  <*#> requiredField "playerGameType" .= [|| playerGameType ||]
  -- <*#> requiredField "previousPlayerGameType" .= [|| previousPlayerGameType ||]
  -- <*#> requiredField "recipeBook" .= [|| recipeBook ||]
  <*#> requiredField "Score" .= [|| score ||]
  <*#> requiredField "XpP" .= [|| xpP ||]
  <*#> requiredField "HealF" .= [|| healF ||]
  <*#> requiredField "SpawnX" .= [|| spawnX ||]
  <*#> requiredField "SpawnY" .= [|| spawnY ||]
  <*#> requiredField "SpawnZ" .= [|| spawnZ ||]
  <*#> requiredField "XpSeed" .= [|| xpSeed ||]
  <*#> requiredField "XpLevel" .= [|| xpLevel ||]
  <*#> requiredField "XpTotal" .= [|| xpTotal ||]
  <*#> requiredField "Sleeping" .= [|| sleeping ||]
  <*#> requiredField "UUIDMost" .= [|| uuidMost ||]
  <*#> requiredField "UUIDLeast" .= [|| uuidLeast ||]
  <*#> requiredField "SelectedItem" .= [|| selectedItem ||]
  <*#> requiredField "SelectedItemSlot" .= [|| selectedItemSlot ||]

