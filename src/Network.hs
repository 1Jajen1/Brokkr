{-# LANGUAGE DataKinds #-}
module Network (
  runProtocol
) where


import qualified Network.Packet.Server.Handshake as S
import Network.Handler.Login

import Network.Handler.Play
import Network.Monad
import qualified Network.Connection as Connection
import qualified Network.Connection.Internal as Connection
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Network.Protocol
import Game.Monad (GameM)
import Util.Log
import Game.State
import Control.Monad.Trans.State.Strict (runStateT)
import Data.UUID
import Util.Exception

runProtocol ::
  ( MonadIO m
  , MonadBracket m
  , MonadNetwork m
  , MonadLog m
  , MonadGameState m -- This is a code smell, this bit should not care about gamestate, just make that part of mkHandle and add a cleanup func?
  ) => (UUID -> Protocol -> IO Connection.Handle) -> m ()
runProtocol mkHandle = do
  S.Handshake _pVersion _addr _addrPort next <- readPacket (Protocol NoCompression NoEncryption)

  ((uid, uname), prot) <- flip runStateT (Protocol NoCompression NoEncryption) $ case next of
    S.Status -> error "Status"
    S.Login -> loginProtocol

  conn <- bracketOnError (liftIO $ mkHandle uid prot) (\conn _ -> liftIO $ Connection.close conn) $ \conn -> joinPlayer conn uid uname

  runReaderT (playProtocol prot) conn
-- This should ideally be at the import place not here
{-# SPECIALIZE runProtocol :: (UUID -> Protocol -> IO Connection.Handle) -> NetworkM (GameM IO) () #-}
