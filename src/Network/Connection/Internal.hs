module Network.Connection.Internal (
  Handle(..)
, DisconnectException(..)
) where

import qualified Network.Packet.Client.Play as C

import qualified Data.Vector as V
import Command.Connection
import Optics
import Data.UUID
import Util.UUID
import Control.Exception
import Data.Text
import Util.Queue (Queue)

data Handle = Handle {
    sendPacket    :: (Int, C.PlayPacket) -> IO ()
  , sendPackets   :: V.Vector (Int, C.PlayPacket) -> IO ()
  , showHandle    :: String
  , commands      :: Queue Command
  , uid           :: UUID
  , close         :: IO ()
  , disconnect    :: Text -> IO ()
}

instance Show Handle where
  show Handle{showHandle} = showHandle
  {-# INLINE show #-}

instance HasUUID Handle where
  uuid = to uid

data DisconnectException = DisconnectException
  deriving stock Show

instance Exception DisconnectException
