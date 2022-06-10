{-# LANGUAGE TemplateHaskell #-}
module Network.Packet.Server.Play (
  PlayPacket(..)
) where
import Util.Binary
import Network.Util.VarNum
import Network.Util.Packet
import Data.Word
import Player
import Util.Position
import Util.Rotation


data PlayPacket =
    TeleportConfirm Int
  | QueryBlockNBT
  | SetDifficulty
  | ChatMessage
  | ClientStatus
  | ClientSettings
  | TabComplete
  | ClickWindowButton
  | ClickWindow
  | CloseWindow
  | PluginMessage
  | EditBook
  | QueryEntityNBT
  | InteractEntity
  | GenerateStructure
  | KeepAlive Word64
  | LockDifficulty
  | PlayerPosition Position OnGround
  | PlayerPositionAndRotation Position Rotation OnGround
  | PlayerRotation Rotation OnGround
  | PlayerMovement OnGround
  deriving stock Show

instance FromBinary PlayPacket where
  get = $$(mkPacketParser [
      [|| teleportConfirm ||]
    , [|| pure QueryBlockNBT ||]
    , [|| pure SetDifficulty ||]
    , [|| pure ChatMessage ||]
    , [|| pure ClientStatus ||]
    , [|| pure ClientSettings ||]
    , [|| pure TabComplete ||]
    , [|| pure ClickWindowButton ||]
    , [|| pure ClickWindow ||]
    , [|| pure CloseWindow ||]
    , [|| pure PluginMessage ||]
    , [|| pure EditBook ||]
    , [|| pure QueryEntityNBT ||]
    , [|| pure InteractEntity ||]
    , [|| pure GenerateStructure ||]
    , [|| KeepAlive <$> get ||]
    , [|| pure LockDifficulty ||]
    , [|| PlayerPosition <$> get <*> get ||]
    , [|| PlayerPositionAndRotation <$> get <*> get <*> get ||]
    , [|| PlayerRotation <$> get <*> get ||]
    , [|| PlayerMovement <$> get ||]
    ])
    where
      teleportConfirm = TeleportConfirm . fromIntegral <$> get @VarInt

