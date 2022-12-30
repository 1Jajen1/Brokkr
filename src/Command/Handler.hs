module Command.Handler (
  Handler(..)
, runHandler
, getServer
, writePacket
, unsafeTellAction
) where

import Control.Concurrent.STM
import Control.Monad.STM.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.CPS

import Network.Connection (Connection, SendPacket(..))
import qualified Network.Connection as Conn

import Network.Packet.Client.Play (Packet)

import Server

-- The base monad is STM and it is to be expected that all handlers run concurrently
-- The sole exception is commands on one connection, they will always be sequential
-- This also extends to the io actions returned. They will always run right after the
--  handler and always before the next command on this connection is handled
newtype Handler a = Handler (ReaderT Server (WriterT Action STM) a)
  deriving newtype (Functor, Applicative, Monad, MonadSTM)

runHandler :: Server -> Handler a -> IO a
runHandler s (Handler f) = do
  (a, Action io) <- atomically $ runWriterT (runReaderT f s)
  io
  pure a

getServer :: Handler Server
getServer = Handler ask

-- TODO Additional method for batched writes
writePacket :: Connection -> Int -> Packet -> Handler ()
writePacket conn sz pack = Handler . lift . tell . Action . Conn.sendPacket conn $ SendPacket sz pack

-- escape hatch to do raw IO in a handler
unsafeTellAction :: IO () -> Handler ()
unsafeTellAction = Handler . lift . tell . Action

--

newtype Action = Action (IO ())

instance Semigroup Action where
  Action f <> Action g = Action $ f >> g

instance Monoid Action where
  mempty = Action $ pure ()
