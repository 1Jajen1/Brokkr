{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Packet.ServerToClient.Play (
  PlayPacket(..)
, module Brokkr.Packet.Common
, module Brokkr.Packet.ServerToClient.Play.Types
) where

import Brokkr.Packet.Binary
import Brokkr.Packet.Common
import Brokkr.Packet.TH
import Brokkr.Packet.SizePrefixed

import Brokkr.Packet.ServerToClient.Play.Types

import Data.Coerce
import Data.Int
import qualified Data.Vector as V

import Language.Haskell.TH
import GHC.TypeLits (KnownNat, Div)

data PlayPacket dimHeight =
    SpawnEntity
  | SpawnExperienceOrb
  | SpawnPlayer !(EntityId VarInt) !UUID !Position !Angle !Angle
  | EntityAnimation
  | AwardStatistics
  | AcknowledgeBlockChange
  | SetBlockDestroyChange
  | BlockEntityData
  | BlockAction
  | BlockUpdate
  | BossBar
  | ChangeDifficulty
  | ClearTitles
  | CommandSuggestionsResponse
  | Commands
  | CloseContainer
  | SetContainerContent
  | SetContainerProperty
  | SetContainerSlot
  | SetCooldown
  | ChatSuggestions
  | PluginMessage
  | DeleteMessage
  | Disconnect
  | DisguisedChatMessage
  | EntityEvent
  | Explosion
  | UnloadChunk !Int32 !Int32
  | GameEvent
  | OpenHorseScreen
  | InitializeWorldBorder
  | KeepAlive !Int64
  | ChunkDataAndUpdateLight !Int32 !Int32 !(HeightMaps dimHeight) !(SectionData dimHeight) !(V.Vector BlockEntity) !TrustEdges !LightData
  | WorldEvent
  | Particle
  | UpdateLight
  | Login
      !(EntityId Int32) !Hardcore !GameMode !PreviousGameMode !(V.Vector DimensionName) !RegistryCodec
      !DimensionType !DimensionName !HashedSeed !MaxPlayers !ViewDistance !SimulationDistance !ReducedDebugInfo !EnableRespawnScreen
      !IsDebug !IsFlat !(Maybe DeathLocation)
  | MapData
  | MerchantOffers
  | UpdateEntityPosition
  | UpdateEntityPositionAndRotation
  | UpdateEntityRotation
  | MoveVehicle
  | OpenBook
  | OpenScreen
  | OpenSignEditor
  | Ping
  | PlaceGhostRecipe
  | PlayerAbilities
  | PlayerChatMessage
  | EndCombat
  | EnterCombat
  | CombatDeath
  | PlayerInfoRemove !PlayerInfoRemoves
  | PlayerInfoUpdate !PlayerInfoUpdates
  | LookAt
  | SynchronizePlayerPosition !Position !Rotation !PosBitField !TeleportId !Dismount
  | UpdateRecipeBook
  | RemoveEntities
  | RemoveEntityEffect
  | ResourcePack
  | Respawn
  | SetHeadRotation
  | UpdateSectionBlocks
  | SelectAdvancementsTab
  | ServerData
  | SetActionBarText
  | SetBorderCenter
  | SetBorderLerpSize
  | SetBorderSize
  | SetBorderWarningDelay
  | SetBorderWarningDistance
  | SetCamera
  | SetHeldItem
  | SetCenterChunk {- VarInt -} !Int32 {- VarInt -} !Int32
  | SetRenderDistance
  | SetDefaultSpawnPosition !Location !SpawnAngle
  | DisplayObjective
  | SetEntityMetadata
  | LinkEntities
  | SetEntityVelocity
  | SetEquipment
  | SetExperience
  | SetHealth
  | UpdateObjectives
  | SetPassengers
  | UpdateTeams
  | UpdateScore
  | SetSimulationDistance
  | SetSubtitleText
  | UpdateTime
  | SetTitleText
  | SetTitleAnimationTimes
  | EntitySoundEffect
  | SoundEffect
  | StopSound
  | SystemChatMessage
  | SetTabListHeaderAndFooter
  | TagQueryResponse
  | PickupItem
  | TeleportEntity
  | UpdateAdvancements
  | UpdateAttributes
  | FeatureFlags
  | EntityEffect
  | UpdateRecipes
  | UpdateTags

deriving stock instance (KnownNat (ToHeightMapSize dimHeight)) => Show (PlayPacket dimHeight)
deriving stock instance (KnownNat (ToHeightMapSize dimHeight)) => Eq (PlayPacket dimHeight)

-- Why? No clue
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/9813
$(return [])

instance KnownNat (ToHeightMapSize dimHeight) => ToBinary (PlayPacket dimHeight) where
  put = $(mkPacketBuilder ''PlayPacket [
      ('ChunkDataAndUpdateLight, do
        nms@[ncX, ncZ, nhm, nsd, nbes, nte, nld] <- sequence [newName "cX", newName "cZ", newName "hm", newName "sd", newName "bes", newName "te", newName "ld"]
        match (conP 'ChunkDataAndUpdateLight (fmap varP nms)) (normalB [|
          put $(varE ncX) <> put $(varE ncZ) <> put $(varE nhm) <> put $(varE nsd) <> put (SizePrefixed @VarInt $(varE nbes)) <> put $(varE nte) <> put $(varE nld)
          |]) [])
    , ('Login, do
        nms@[neid, nhc, ngm, npgm, ndm, nreg, ndt, ndn, nhs, nmp, nvd, nsd, nrd, nersp, nisdbg, nisflt, ndl] <- sequence
          [ newName "eid", newName "hc", newName "gm", newName "pgm", newName "dm", newName "reg", newName "dt", newName "dn"
          , newName "hs", newName "mp", newName "vd", newName "sd", newName "rd", newName "ersp", newName "isdbg", newName "isflt"
          , newName "dl"
          ]
        match (conP 'Login (fmap varP nms)) (normalB [|
             put $(varE neid) <> put $(varE nhc) <> put $(varE ngm) <> put $(varE npgm) <> put (SizePrefixed @VarInt $(varE ndm))
          <> put $(varE nreg) <> put $(varE ndt) <> put $(varE ndn) <> put $(varE nhs) <> put $(varE nmp)
          <> put $(varE nvd)  <> put $(varE nsd) <> put $(varE nrd) <> put $(varE nersp) <> put $(varE nisdbg)
          <> put $(varE nisflt) <> put $(varE ndl)
          |]) [])
    , ('SetCenterChunk, do
        nms@[ncX, ncZ] <- sequence [newName "cX", newName "cZ"]
        match (conP 'SetCenterChunk (fmap varP nms)) (normalB [|
          put (VarInt $(varE ncX)) <> put (VarInt $(varE ncZ))
          |]) [])
    ])
  {-# INLINE put #-}

instance (KnownNat (Div dimHeight 16), KnownNat (ToHeightMapSize dimHeight)) => FromBinary (PlayPacket dimHeight) where
  get = $(mkPacketParser ''PlayPacket [
      ('ChunkDataAndUpdateLight, [| ChunkDataAndUpdateLight
          <$> get <*> get <*> get <*> get
          <*> (coerce @(SizePrefixed VarInt (V.Vector BlockEntity)) <$> get)
          <*> get <*> get |])
    , ('Login, [| Login
          <$> get <*> get <*> get <*> get
          <*> (coerce @(SizePrefixed VarInt (V.Vector DimensionName)) <$> get)
          <*> get <*> get <*> get <*> get <*> get <*> get
          <*> get <*> get <*> get <*> get <*> get <*> get |])
    , ('SetCenterChunk, [| with @VarInt $ \cX -> with @VarInt $ \cZ -> pure $ SetCenterChunk (fromIntegral cX) (fromIntegral cZ) |])
    ])
