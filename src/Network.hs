{-# LANGUAGE DataKinds #-}
module Network (
  runProtocol
) where


import qualified Network.Packet.Server.Handshake as S
import Network.Handler.Login

import Network.Handler.Play
import Network.Monad
import qualified Network.Connection as Connection
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Network.Protocol
import Game.Monad (GameM)
import Util.Log
import Game.State
import Optics

runProtocol ::
  ( MonadIO m
  , MonadNetwork m
  , MonadLog m
  , MonadGameState m
  ) => (Protocol -> IO Connection.Handle) -> m ()
runProtocol mkHandle = do
  S.Handshake _pVersion _addr _addrPort next <- readPacket

  (uid, uname) <- case next of
    S.Status -> error "Status"
    S.Login -> loginProtocol

  conn <- liftIO $ mkHandle (Protocol NoCompression NoEncryption)
 
  takeGameState >>= \st -> putGameState $ connection uid .~ (Just conn) $ st

  runReaderT (playProtocol uid uname) conn
{-# SPECIALIZE runProtocol :: (Protocol -> IO Connection.Handle) -> NetworkM (GameM IO) () #-}
