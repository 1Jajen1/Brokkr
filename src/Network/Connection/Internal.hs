module Network.Connection.Internal (
  Handle(..)
) where

import qualified Network.Packet.Client.Play as C

import qualified Data.Vector as V
import Game.Command
import Optics
import Data.UUID
import Util.UUID

data Handle = Handle {
    sendPacket    :: (Int, C.PlayPacket) -> IO ()
  , sendPackets   :: V.Vector (Int, C.PlayPacket) -> IO ()
  , showHandle    :: String
  , pushCommand   :: Command -> IO ()
  , pushCommands  :: V.Vector Command -> IO ()
  , flushCommands :: IO (V.Vector Command)
  , uid          :: UUID 
}

instance Show Handle where
  show Handle{showHandle} = showHandle
  {-# INLINE show #-}

instance HasUUID Handle where
  uuid = to uid
