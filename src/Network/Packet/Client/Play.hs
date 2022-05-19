module Network.Packet.Client.Play (
  PlayPacket(..)
, TeleportId(..)
, Dismount(..)
) where
import Util.Binary
import Network.Util.Packet
import Network.Packet.Client.Play.JoinGame
import Network.Packet.Client.Play.ChunkData
import Block.Position (BlockPosition)
import Util.Position
import Network.Util.FromIntegral
import Network.Util.VarNum
import Util.Rotation
import Data.Word

data PlayPacket =
    SpawnEntity
  | SpawnExperienceOrb
  | SpawnLivingEntity
  | SpawnPainting
  | SpawnPlayer
  | SculkVibration
  | EntityAnimation
  | Statistics
  | AckPlayerDigging
  | BlockBreakAnimation
  | BlockEntityData
  | BlockAction
  | BlockChange
  | BossBar
  | ServerDifficulty
  | ChatMessage
  | ClearTitles
  | TabComplete
  | DeclareCommands
  | CloseWindow
  | WindowItems
  | WindowProperty
  | SetSlot
  | SetCooldown
  | PluginMessage
  | NamedSoundEffect
  | Disconnect
  | EntityStatus
  | Explosion
  | UnloadChunk
  | ChangeGameState
  | OpenHorseWindow
  | InitializeWorldBorder
  | KeepAlive
  | ChunkDataAndUpdateLight ChunkData
  | Effect
  | Particle
  | UpdateLight
  | JoinGame JoinGameData
  | MapData
  | TradeList
  | EntityPosition
  | EntityPositionAndRotation
  | EntityRotation
  | VehicleMove
  | OpenBook
  | OpenWindow
  | OpenSignEditor
  | Ping
  | CraftRecipeResponse
  | PlayerAbilities
  | EndCombatEvent
  | EnterCombatEvent
  | DeathCombatEvent
  | PlayerInfo
  | FacePlayer
  | PlayerPositionAndLook Position Rotation TeleportId Dismount
  | UnlockRecipes
  | DestroyEntities
  | RemoveEntityEffect
  | ResourcePackSend
  | Respawn
  | EntityHeadLook
  | MultiBlockChange
  | SelectAdvancementTab
  | ActionBar
  | WorldBorderCenter
  | WorldBorderLerpSize
  | WorldBorderSize
  | WorldBorderWarningDelay
  | WorldBorderWarningReach
  | Camera
  | HeldItemChange
  | UpdateViewPosition Int Int
  | UpdateViewDistance
  | SpawnPosition BlockPosition Float
  | DisplayScoreboard
  | EntityMetadata
  | AttachEntity
  | EntityVelocity
  | EntityEquipment
  | SetExperience
  | UpdateHealth
  | ScoreboardObjective
  | SetPassengers
  | Teams
  | UpdateScore
  | UpdateSimulationDistance
  | SetTitleSubTitle
  | TimeUpdate
  | SetTitleText
  | SetTitleTimes
  | EntitySoundEffect
  | SoundEffect
  | StopSound
  | PlayerListHeaderAndFooter
  | NBTQueryResponse
  | CollectItem
  | EntityTeleport
  | Advancements
  | EntityProperties
  | EntityEffect
  | DeclareRecipes
  | Tags
  deriving stock Show

instance ToBinary PlayPacket where
  put a = packetId a <> case a of
    ChunkDataAndUpdateLight d -> put d
    JoinGame d -> put d
    PlayerPositionAndLook pos rot tId dism -> put pos <> put rot <> put (0 :: Word8) <> put tId <> put dism
    UpdateViewPosition chunkX chunkY -> put (VarInt $ fromIntegral chunkX) <> put (VarInt $ fromIntegral chunkY)
    SpawnPosition pos angle -> put pos <> put angle
    _ -> error "Unsupported"

newtype TeleportId = TeleportId Int
  deriving stock Show
  deriving ToBinary via FromIntegral Int VarInt

data Dismount = Dismount | NoDismount
  deriving stock Show

instance ToBinary Dismount where
  put Dismount = put True
  put NoDismount = put False

