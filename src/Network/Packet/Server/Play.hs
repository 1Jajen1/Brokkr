{-# LANGUAGE TemplateHaskell #-}
module Network.Packet.Server.Play (
  Packet(..)
) where

import Util.Binary
import Network.Util
import Network.Util.Packet
import Data.Int
import Util.Position
import Util.Rotation

import Data.ByteString (ByteString)
import Data.Coerce
import Data.Functor
import Data.Text (Text)

import FlatParse.Basic (takeRestBs)

import Network.Util.EntityId

import Network.Packet.Server.Play.ClientInformation
import Network.Packet.Server.Play.PlayerAbilities
import Network.Packet.Server.Play.PlayerCommand

data Packet =
    ConfirmTeleportation Int
  | QueryBlockEntityTag
  | ChangeDifficulty
  | MessageAcknowledgment
  | ChatCommand
  | ChatMessage
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
  | SetPlayerPosition Position Falling
  | SetPlayerPositionAndRotation Position Rotation Falling
  | SetPlayerRotation Rotation Falling
  | SetPlayerOnGround Falling
  | MoveVehicle
  | PaddleBoat
  | PickItem
  | PlaceRecipe
  | PlayerAbilities Abilities
  | PlayerAction
  | PlayerCommand !(EntityId VarInt) !PlayerCommand
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
    , [|| PlayerCommand <$> get <*> get ||]
    ])
    where
      confirmTeleport = ConfirmTeleportation . fromIntegral <$> get @VarInt
  {-# INLINE get #-}
