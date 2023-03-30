module Network.Packet.Client.Play (
  Packet(..)
, TeleportId(..)
, Dismount(..)
) where
import Util.Binary
import Network.Util.Packet
import Network.Packet.Client.Play.Login
import Network.Packet.Client.Play.ChunkData
import Block.Position (BlockPosition)
import Util.Position
import Network.Util.FromIntegral
import Network.Util.VarNum
import Util.Rotation
import Data.Word
import Data.Int
import Data.Text
import Network.Util.MCString

data Packet =
    SpawnEntity
  | SpawnExperienceOrb
  | SpawnPlayer
  | EntityAnimation
  | AwardStatistics
  | AckBlockChange
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
  | Disconnect !Text
  | DisguisedChatMessage
  | EntityEvent
  | Explosion
  | UnloadChunk !Int !Int
  | GameEvent
  | OpenHorseScreen
  | InitializeWorldBorder
  | KeepAlive !Int64
  | ChunkDataAndUpdateLight {-# UNPACk #-} !ChunkData
  | WorldEvent
  | Particle
  | UpdateLight
  | Login !LoginData -- Some changes here
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
  | PlayerInfoRemove
  | PlayerInfoUpdate
  | LookAt
  | SynchronizePlayerPosition !Position !Rotation !TeleportId !Dismount
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
  | SetBorderWarningReach
  | SetCamera
  | SetHeldItem
  | SetCenterChunk !Int !Int
  | SetRenderDistance
  | SetDefaultSpawnPosition !BlockPosition !Float
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
  deriving stock Show

instance ToBinary Packet where
  put a = packetId a <> case a of
    ChunkDataAndUpdateLight d -> put d
    Login d -> put d
    SynchronizePlayerPosition pos rot tId dism -> put pos <> put rot <> put (0 :: Word8) <> put tId <> put dism
    SetCenterChunk chunkX chunkZ -> put (VarInt $ fromIntegral chunkX) <> put (VarInt $ fromIntegral chunkZ)
    SetDefaultSpawnPosition pos angle -> put pos <> put angle
    KeepAlive w -> put w
    UnloadChunk x z -> put (fromIntegral @_ @Int32 x) <> put (fromIntegral @_ @Int32 z)
    Disconnect t -> put (MCString t)
    _ -> error "Unsupported"
  {-# INLINE put #-}

newtype TeleportId = TeleportId Int
  deriving stock Show
  deriving ToBinary via FromIntegral Int VarInt

data Dismount = Dismount | NoDismount
  deriving stock Show

instance ToBinary Dismount where
  put Dismount = put True
  put NoDismount = put False

