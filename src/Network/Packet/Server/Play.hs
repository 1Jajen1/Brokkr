{-# LANGUAGE TemplateHaskell #-}
module Network.Packet.Server.Play (
  PlayPacket(..)
) where
import Util.Binary
import Network.Util.VarNum
import Network.Util.Packet


data PlayPacket =
    TeleportConfirm Int
  | QueryBlockNBT
  | SetDifficulty
  | ChatMessage
  | ClientStatus
  | ClientSettings
  deriving stock Show

instance FromBinary PlayPacket where
  get = $$(mkPacketParser [
      [|| teleportConfirm ||]
    , [|| pure QueryBlockNBT ||]
    , [|| pure SetDifficulty ||]
    , [|| pure ChatMessage ||]
    , [|| pure ClientStatus ||]
    , [|| pure ClientSettings ||]
    , [|| pure QueryBlockNBT ||]
    , [|| pure SetDifficulty ||]
    , [|| pure ChatMessage ||]
    , [|| pure ClientStatus ||]
    , [|| pure ClientSettings ||]
    , [|| pure QueryBlockNBT ||]
    , [|| pure SetDifficulty ||]
    , [|| pure ChatMessage ||]
    , [|| pure ClientStatus ||]
    , [|| pure ClientSettings ||]
    , [|| pure QueryBlockNBT ||]
    , [|| pure SetDifficulty ||]
    , [|| pure ChatMessage ||]
    , [|| pure ClientStatus ||]
    , [|| pure ClientSettings ||]
    , [|| pure QueryBlockNBT ||]
    , [|| pure SetDifficulty ||]
    , [|| pure ChatMessage ||]
    , [|| pure ClientStatus ||]
    , [|| pure ClientSettings ||]
    , [|| pure QueryBlockNBT ||]
    , [|| pure SetDifficulty ||]
    , [|| pure ChatMessage ||]
    , [|| pure ClientStatus ||]
    , [|| pure ClientSettings ||]
    ])
    where
      teleportConfirm = TeleportConfirm . fromIntegral <$> get @VarInt

