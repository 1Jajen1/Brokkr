{-# LANGUAGE TemplateHaskell #-}
module Network.Packet.Server.Play (
  Packet(..)
) where

import Block.BlockFace
import Block.Position

import Data.Int
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Text (Text)
import Data.Word
import Data.Bitfield (Bitfield)

import Entity.Util.Hand

import FlatParse.Basic (takeRest)

import Network.Packet.Server.Play.ClientInformation
import Network.Packet.Server.Play.PlayerAbilities
import Network.Packet.Server.Play.PlayerAction
import Network.Packet.Server.Play.PlayerCommand
import Network.Packet.Server.Play.UseItemOn

import Network.Util
import Network.Util.Packet

import Util.Binary
import Util.Position
import Util.Rotation

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
  | CloseContainer !Word8
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
  | PlayerAbilities !(Bitfield Word8 Abilities)
  | PlayerAction !PlayerAction !VarInt -- TODO Newtype the varint to Seqence
  | PlayerCommand !(EntityId VarInt) !PlayerCommand
  | PlayerInput
  | Pong
  | PlayerSession
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
  | SetCreativeModeSlot !SlotIndex !Slot
  | ProgramJigsawBlock
  | ProgramStructureBlock
  | UpdateSign
  | SwingArm !Hand
  | TeleportToEntity
  | UseItemOn !Hand !BlockPosition !BlockFace !Cursor !HeadInBlock !VarInt -- TODO Newtype VarInt to Seqence
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
    , [|| CloseContainer <$> get ||]
    , [|| PluginMessage . coerce <$> get @MCString <*> takeRest ||]
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
    , [|| PlayerAction <$> get <*> get ||]
    , [|| PlayerCommand <$> get <*> get ||]
    , [|| pure PlayerInput ||]
    , [|| pure Pong ||]
    , [|| pure PlayerSession ||]
    , [|| pure ChangeRecipeBookSettings ||]
    , [|| pure SetSeenRecipe ||]
    , [|| pure RenameItem ||]
    , [|| pure ResourcePack ||]
    , [|| pure SeenAdvancements ||]
    , [|| pure SelectTrade ||]
    , [|| pure SetBeaconEffect ||]
    , [|| pure SetHeldItem ||]
    , [|| pure ProgramCommandBlock ||]
    , [|| pure ProgramCommandBlockMinecart ||]
    , [|| SetCreativeModeSlot <$> get <*> get ||]
    , [|| pure ProgramJigsawBlock ||]
    , [|| pure ProgramStructureBlock ||]
    , [|| pure UpdateSign ||]
    , [|| SwingArm <$> get ||]
    , [|| pure TeleportToEntity ||]
    , [|| UseItemOn <$> get <*> get <*> get <*> get <*> get <*> get ||]
    , [|| pure UseItem ||]
    ])
    where
      confirmTeleport = ConfirmTeleportation . fromIntegral <$> get @VarInt
  {-# INLINE get #-}

instance ToBinary Packet where
  put p = packetId p <> case p of
    SetPlayerOnGround onG -> put onG
    SetPlayerPosition pos onG -> put pos <> put onG
    SetPlayerRotation rot onG -> put rot <> put onG
    SetPlayerPositionAndRotation pos rot onG -> put pos <> put rot <> put onG
    _ -> error "TODO"
