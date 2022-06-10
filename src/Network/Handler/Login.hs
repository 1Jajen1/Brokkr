module Network.Handler.Login (
  loginProtocol
) where

import qualified Network.Packet.Server.Login as S
import qualified Network.Packet.Client.Login as C
import qualified Data.UUID as UUID
import qualified Data.UUID.V3 as V3
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import Data.UUID
import Data.Text
import Network.Monad
import Game.Monad (GameM)
import Util.Log

loginProtocol ::
  ( MonadNetwork m
  , MonadLog m
  ) => m (UUID, Text)
loginProtocol = readPacket >>= \case
  S.LoginStart uName -> do
    let uid = V3.generateNamed UUID.nil (BS.unpack $ T.encodeUtf8 uName)

    sendPacket 64 $ C.LoginSuccess uid uName

    logInfo $ "Player " <> uName <> " logged in."

    return (uid, uName)
{-# SPECIALIZE loginProtocol :: NetworkM (GameM IO) (UUID, Text) #-}
