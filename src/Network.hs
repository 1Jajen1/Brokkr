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
import Control.Monad.Trans.State.Strict (runStateT)

runProtocol ::
  ( MonadIO m
  , MonadNetwork m
  , MonadLog m
  , MonadGameState m
  ) => (Protocol -> IO Connection.Handle) -> m ()
runProtocol mkHandle = do
  S.Handshake _pVersion _addr _addrPort next <- readPacket (Protocol NoCompression NoEncryption)

  ((uid, uname), prot) <- flip runStateT (Protocol NoCompression NoEncryption) $ case next of
    S.Status -> error "Status"
    S.Login -> loginProtocol

  conn <- liftIO $ mkHandle prot
 
  takeGameState >>= \st -> putGameState $ connection uid .~ (Just conn) $ st

  runReaderT (playProtocol prot uid uname) conn
{-# SPECIALIZE runProtocol :: (Protocol -> IO Connection.Handle) -> NetworkM (GameM IO) () #-}
