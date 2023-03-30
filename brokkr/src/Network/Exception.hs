module Network.Exception (
  ConnectionClosed(..)
, PacketFailure(..)
, Showable(..)
, InvalidProtocol(..)
, ClientTimedOut(..)
, InvalidKeepAlive(..)
) where

import Control.Exception

import Data.ByteString
import Data.Int
import Data.String
import Data.Text

-- Exceptions

data ConnectionClosed = ConnectionClosed
  deriving stock Show

instance Exception ConnectionClosed

data PacketFailure = FailedParse ByteString | FailedDecompress ByteString | FailedExtraBytes Showable ByteString
  deriving stock Show

instance Exception PacketFailure

data Showable = forall a . Show a => Showable a

instance Show Showable where
  show (Showable a) = show a

data InvalidProtocol = UnexpectedPacket Expected Unexpected
  deriving stock Show

instance Exception InvalidProtocol

newtype Expected = Expected Text
  deriving stock Show
  deriving newtype IsString

newtype Unexpected = Unexpected Text
  deriving stock Show
  deriving newtype IsString

data ClientTimedOut = ClientTimedOut
  deriving stock Show

instance Exception ClientTimedOut

data InvalidKeepAlive = InvalidKeepAlive Int64 Int64 | UnexpectedKeepAlive
  deriving stock Show

instance Exception InvalidKeepAlive
