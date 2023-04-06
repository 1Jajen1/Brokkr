{-# LANGUAGE TemplateHaskell #-}
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
import Network.Util.VarNum
import Util.Rotation
import Data.Word
import Data.Int
import Data.Text
import Network.Util.MCString
import Data.Coerce
import Util.TeleportId
import Util.Dismount
-- TODO ^^

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
  deriving stock (Eq,Show)

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

instance FromBinary Packet where
  get = $$(mkPacketParser [
      [|| pure SpawnEntity ||]
    , [|| pure SpawnExperienceOrb ||]
    , [|| pure SpawnPlayer ||]
    , [|| pure EntityAnimation ||]
    , [|| pure AwardStatistics ||]
    , [|| pure AckBlockChange ||]
    , [|| pure SetBlockDestroyChange ||]
    , [|| pure BlockEntityData ||]
    , [|| pure BlockAction ||]
    , [|| pure BlockUpdate ||]
    , [|| pure BossBar ||]
    , [|| pure ChangeDifficulty ||]
    , [|| pure ClearTitles ||]
    , [|| pure CommandSuggestionsResponse ||]
    , [|| pure Commands ||]
    , [|| pure CloseContainer ||]
    , [|| pure SetContainerContent ||]
    , [|| pure SetContainerProperty ||]
    , [|| pure SetContainerSlot ||]
    , [|| pure SetCooldown ||]
    , [|| pure ChatSuggestions ||]
    , [|| pure PluginMessage ||]
    , [|| pure DeleteMessage ||]
    , [|| Disconnect <$> (coerce <$> get @MCString) ||]
    , [|| pure DisguisedChatMessage ||]
    , [|| pure EntityEvent ||]
    , [|| pure Explosion ||]
    , [|| UnloadChunk <$> (fromIntegral <$> get @VarInt) <*> (fromIntegral <$> get @VarInt) ||]
    , [|| pure GameEvent ||]
    , [|| pure OpenHorseScreen ||]
    , [|| pure InitializeWorldBorder ||]
    , [|| KeepAlive <$> get ||]
    , [|| ChunkDataAndUpdateLight <$> get ||]
    , [|| pure WorldEvent ||]
    , [|| pure Particle ||]
    , [|| pure UpdateLight ||]
    , [|| Login <$> get ||]
    , [|| pure MapData ||]
    , [|| pure MerchantOffers ||]
    , [|| pure UpdateEntityPosition ||]
    , [|| pure UpdateEntityPositionAndRotation ||]
    , [|| pure UpdateEntityRotation ||]
    , [|| pure MoveVehicle ||]
    , [|| pure OpenBook ||]
    , [|| pure OpenScreen ||]
    , [|| pure OpenSignEditor ||]
    , [|| pure Ping ||]
    , [|| pure PlaceGhostRecipe ||]
    , [|| pure PlayerAbilities ||]
    , [|| pure PlayerChatMessage ||]
    , [|| pure EndCombat ||]
    , [|| pure EnterCombat ||]
    , [|| pure CombatDeath ||]
    , [|| pure PlayerInfoRemove ||]
    , [|| pure PlayerInfoUpdate ||]
    , [|| pure LookAt ||]
    , [|| SynchronizePlayerPosition <$> get <*> get <*> (get @Int8 >> get) <*> get ||]
    , [|| pure UpdateRecipeBook ||]
    , [|| pure RemoveEntities ||]
    , [|| pure RemoveEntityEffect ||]
    , [|| pure ResourcePack ||]
    , [|| pure Respawn ||]
    , [|| pure SetHeadRotation ||]
    , [|| pure UpdateSectionBlocks ||]
    , [|| pure SelectAdvancementsTab ||]
    , [|| pure ServerData ||]
    , [|| pure SetActionBarText ||]
    , [|| pure SetBorderCenter ||]
    , [|| pure SetBorderLerpSize ||]
    , [|| pure SetBorderSize ||]
    , [|| pure SetBorderWarningDelay ||]
    , [|| pure SetBorderWarningReach ||]
    , [|| pure SetCamera ||]
    , [|| pure SetHeldItem ||]
    , [|| SetCenterChunk <$> (fromIntegral <$> get @VarInt) <*> (fromIntegral <$> get @VarInt) ||]
    , [|| pure SetRenderDistance ||]
    , [|| SetDefaultSpawnPosition <$> get <*> get ||]
    , [|| pure DisplayObjective ||]
    , [|| pure SetEntityMetadata ||]
    , [|| pure LinkEntities ||]
    , [|| pure SetEntityVelocity ||]
    , [|| pure SetEquipment ||]
    , [|| pure SetExperience ||]
    , [|| pure SetHealth ||]
    , [|| pure UpdateObjectives ||]
    , [|| pure SetPassengers ||]
    , [|| pure UpdateTeams ||]
    , [|| pure UpdateScore ||]
    , [|| pure SetSimulationDistance ||]
    , [|| pure SetSubtitleText ||]
    , [|| pure UpdateTime ||]
    , [|| pure SetTitleText ||]
    , [|| pure SetTitleAnimationTimes ||]
    , [|| pure EntitySoundEffect ||]
    , [|| pure SoundEffect ||]
    , [|| pure StopSound ||]
    , [|| pure SystemChatMessage ||]
    , [|| pure SetTabListHeaderAndFooter ||]
    , [|| pure TagQueryResponse ||]
    , [|| pure PickupItem ||]
    , [|| pure TeleportEntity ||]
    , [|| pure UpdateAdvancements ||]
    , [|| pure UpdateAttributes ||]
    , [|| pure FeatureFlags ||]
    , [|| pure EntityEffect ||]
    , [|| pure UpdateRecipes ||]
    , [|| pure UpdateTags ||]
    ])
