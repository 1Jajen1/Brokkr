module Network.Handler.Login (
  loginProtocol
) where

import Effectful
import Effectful.State.Static.Local (State)
import Network.Effect.Network
import Data.ByteString
import Network.Effect.Packet
import qualified Network.Packet.Server.Login as S
import qualified Network.Packet.Client.Login as C
import Util.Binary
import qualified Data.UUID as UUID
import qualified Data.UUID.V3 as V3
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import Data.UUID
import Data.Text

loginProtocol :: (Network :> es, State ByteString :> es) => Eff es (UUID, Text)
loginProtocol = readPacket get >>= \case
  S.LoginStart uName -> do
    let uid = V3.generateNamed UUID.nil (BS.unpack $ T.encodeUtf8 uName)
    sendPacket 64 $ C.LoginSuccess uid uName
    return (uid, uName)
