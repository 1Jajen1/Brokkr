{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module Brokkr.Packet.ClientToServer.Play (
  PlayPacket(..)
, module Brokkr.Packet.ClientToServer.Play.Types
, module Brokkr.Packet.Common
) where

import Brokkr.Packet.Binary
import Brokkr.Packet.SizePrefixed
import Brokkr.Packet.ClientToServer.Play.Types
import Brokkr.Packet.Common
import Brokkr.Packet.TH

import Data.Coerce
import Data.Int (Int64, Int32, Int16)
import Data.Vector qualified as V

import Language.Haskell.TH

data PlayPacket =
    ConfirmTeleportation !TeleportId
  | QueryBlockEntityTag !TransactionId !Location
  | ChangeDifficulty !Difficulty
  | MessageAcknowledgment !MessageCount
  | ChatCommand !Command !Timestamp !Salt !ArgumentSignatures !MessageCount !Acknowledged
  | ChatMessage !Message !Timestamp !Salt !(Maybe Signature)  !MessageCount !Acknowledged
  | ClientCommand !ActionId
  | ClientInformation !Locale !ViewDistance !ChatMode !UseChatColors !DisplayedSkinParts !MainHand !UseTextFiltering !AllowServerListings
  | CommandSuggestionsRequest !TransactionId !CommandSuggestionsReqText
  | ClickContainerButton !WindowId !ButtonId
  | ClickContainer !WindowId !StateId !ContainerClickAction !(V.Vector IndexedSlot) !Slot
  | CloseContainer !WindowId
  | PluginMessage !Identifier !PluginMessageData
  | EditBook !(SlotIndex VarInt) !(V.Vector PageText) !(Maybe BookTitle)
  | QueryEntityTag !TransactionId !(EntityId VarInt)
  | Interact !(EntityId VarInt) !InteractType !IsSneaking
  | JigsawGenerate !Location !JigsawLevels !KeepJigsaws
  | KeepAlive !Int64
  | LockDifficulty !Lock
  | SetPlayerPosition !Position !OnGround
  | SetPlayerPositionAndRotation !Position !Rotation !OnGround
  | SetPlayerRotation !Rotation !OnGround
  | SetPlayerOnGround !OnGround
  | MoveVehicle !Position !Rotation
  | PaddleBoat !PaddleTurning !PaddleTurning
  | PickItem !(SlotIndex VarInt)
  | PlaceRecipe !WindowId !RecipeId !MakeAll
  | PlayerAbilities !PlayerAbilities
  | PlayerAction !ActionStatus !Location !BlockFace !SequenceId
  | PlayerCommand !(EntityId VarInt) !PlayerCommandAction !HorseJumpBoost
  | PlayerInput !InputSideways !InputForward !InputFlags
  | Pong !Int32
  | PlayerSession !SessionId !ExpiresAt !PublicKey !KeySignature
  | ChangeRecipeBookSettings !RecipeBookId !BookOpen !FilterActive
  | SetSeenRecipe !RecipeId
  | RenameItem !ItemName
  | ResourcePack !ResourcePackStatus
  | SeenAdvancements !AdvancementTabAction
  | SelectTrade !(SlotIndex VarInt)
  | SetBeaconEffect !HasEffect !PotionEffect !HasEffect !PotionEffect
  | SetHeldItem !(SlotIndex Int16) -- TODO Constrain this more to 0-8
  | ProgramCommandBlock -- TODO
  | ProgramCommandBlockMinecart -- TODO
  | SetCreativeModeSlot !(SlotIndex Int16) !Slot
  | ProgramJigsawBlock -- TODO
  | ProgramStructureBlock -- TODO
  | UpdateSign !Location !SignLine !SignLine !SignLine !SignLine
  | SwingArm !Hand
  | TeleportToEntity !TargetPlayer
  | UseItemOn !Hand !Location !BlockFace !Cursor !InsideBlock !SequenceId
  | UseItem !Hand !SequenceId
  deriving stock Show

-- Why? No clue
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/9813
$(return [])

instance ToBinary PlayPacket where
  put = $(mkPacketBuilder ''PlayPacket [
      ('ClickContainer, do
        nms@[nwid, nsid, nact, nslots, ncarriedSlot] <- sequence [newName "wid", newName "sid", newName "act", newName "slots", newName "carriedSlot"]
        match (conP 'ClickContainer (fmap varP nms)) (normalB [|
          put $(varE nwid) <> put $(varE nsid) <> put $(varE nact) <> put (SizePrefixed @VarInt $(varE nslots)) <> put $(varE ncarriedSlot)
          |]) [])
    , ('EditBook, do
        nms@[nsid, npages, ntitle] <- sequence [newName "sid", newName "pages", newName "title"]
        match (conP 'EditBook (fmap varP nms)) (normalB [|
          put $(varE nsid) <> put (SizePrefixed @VarInt $(varE npages)) <> put $(varE ntitle)
          |]) [])
    ])
  {-# INLINE put #-}

instance FromBinary PlayPacket where
  get = $(mkPacketParser ''PlayPacket [
      ('ClickContainer, [| ClickContainer <$> get <*> get <*> get <*> (coerce @(SizePrefixed VarInt (V.Vector IndexedSlot)) <$> get) <*> get |])
    , ('EditBook, [| EditBook <$> get <*> (coerce @(SizePrefixed VarInt (V.Vector PageText)) <$> get) <*> get |])
    ])
