{-# LANGUAGE TemplateHaskell #-}
module Network.Packet.Server.Play (
  Packet(..)
) where

import Util.Binary
import Network.Util.VarNum
import Network.Util.MCString
import Network.Util.Packet
import Data.Word
import Data.Int
import Client
import Util.Position
import Util.Rotation

import Data.ByteString (ByteString)
import Data.Coerce
import Data.Text (Text)

import FlatParse.Basic (takeRestBs)

import Network.Packet.Server.Play.ClientInformation
import Network.Packet.Server.Play.PlayerAbilities

data Packet =
    ConfirmTeleportation Int
  | QueryBlockEntityTag
  | ChangeDifficulty
  | MessageAcknowledgment
  | ChatCommand
  | ChatMessage
  | ChatPreview
  | ClientCommand
  | ClientInformation Locale ViewDistance ChatMode UseChatColors DisplayedSkinParts MainHand UseTextFiltering AllowServerListings
  | CommandSuggestionsRequest
  | ClickContainerButton
  | ClickContainer
  | CloseContainer
  | PluginMessage Text ByteString
  | EditBook
  | QueryEntityTag
  | Interact
  | JigsawGenerate
  | KeepAlive Int64
  | LockDifficulty
  | SetPlayerPosition Position OnGround
  | SetPlayerPositionAndRotation Position Rotation OnGround
  | SetPlayerRotation Rotation OnGround
  | SetPlayerOnGround OnGround
  | MoveVehicle
  | PaddleBoat
  | PickItem
  | PlaceRecipe
  | PlayerAbilities Abilities
  | PlayerAction
  | PlayerCommand
  | PlayerInput
  | Pong
  | ChangeRecipeBookSettings
  | SetSeenRecipe
  | RenameItem
  | ResourcePack
  | SeenAdvancements
  | SelectTrade
  | SetBeaconEffect
  | SetHeldItem
  | ProgramCommandBlock
  | ProgramCommandBlockMinecart
  | SetCreativeModeSlot
  | ProgramJigsawBlock
  | ProgramStructureBlock
  | UpdateSign
  | SwingArm
  | TeleportToEntity
  | UseItemOn
  | UseItem
  deriving stock Show

instance FromBinary Packet where
  get = $$(mkPacketParser [
      [|| confirmTeleport ||]
    , [|| pure QueryBlockEntityTag ||]
    , [|| pure ChangeDifficulty ||]
    , [|| pure MessageAcknowledgment ||]
    , [|| pure ChatCommand ||]
    , [|| pure ChatMessage ||]
    , [|| pure ChatPreview ||]
    , [|| pure ClientCommand ||]
    , [|| ClientInformation <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get ||]
    , [|| pure CommandSuggestionsRequest ||]
    , [|| pure ClickContainerButton ||]
    , [|| pure ClickContainer ||]
    , [|| pure CloseContainer ||]
    , [|| PluginMessage . coerce <$> get @MCString <*> takeRestBs ||]
    , [|| pure EditBook ||]
    , [|| pure QueryEntityTag ||]
    , [|| pure Interact ||]
    , [|| pure JigsawGenerate ||]
    , [|| KeepAlive <$> get ||]
    , [|| pure LockDifficulty ||]
    , [|| SetPlayerPosition <$> get <*> get ||]
    , [|| SetPlayerPositionAndRotation <$> get <*> get <*> get ||]
    , [|| SetPlayerRotation <$> get <*> get ||]
    , [|| SetPlayerOnGround <$> get ||]
    , [|| pure MoveVehicle ||]
    , [|| pure PaddleBoat ||]
    , [|| pure PickItem ||]
    , [|| pure PlaceRecipe ||]
    , [|| PlayerAbilities <$> get ||]
    , [|| pure PlayerAction ||]
    ])
    where
      confirmTeleport = ConfirmTeleportation . fromIntegral <$> get @VarInt
  {-# INLINE get #-}
