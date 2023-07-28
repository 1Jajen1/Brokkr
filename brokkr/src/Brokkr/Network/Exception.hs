module Brokkr.Network.Exception (
  ConnectionClosed(..)
, PacketFailure(..)
, Showable(..)
, InvalidProtocol(..)
, ClientTimedOut(..)
, InvalidKeepAlive(..)
, HexByteString(..)
) where

import Control.Exception

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Int
import Data.String
import Data.Text
import Data.Word

import Foreign.Storable

import GHC.Ptr

-- Exceptions

data ConnectionClosed = ConnectionClosed
  deriving stock Show

instance Exception ConnectionClosed

newtype HexByteString = HexBS ByteString

instance Show HexByteString where
  show (HexBS bs) = show $ toHex bs

-- https://stackoverflow.com/questions/10099921/efficiently-turn-a-bytestring-into-a-hex-representation
maxLen :: Int
maxLen = maxBound `quot` 2

hexDig :: Word8 -> Word8
hexDig d
    | d < 10    = d + 48
    | otherwise = d + 87

toHex :: ByteString -> ByteString
toHex bs
    | len > maxLen = error "too long to convert"
    | otherwise    = BS.unsafeCreate nl (go 0)
      where
        len = BS.length bs
        nl  = 2*len
        go i p
            | i == len  = return ()
            | otherwise = case BS.unsafeIndex bs i of
                            w -> do poke p (hexDig $ w `shiftR` 4)
                                    poke (p `plusPtr` 1) (hexDig $ w .&. 0xF)
                                    go (i+1) (p `plusPtr` 2)


data PacketFailure = FailedParse HexByteString | FailedDecompress HexByteString | FailedExtraBytes Showable HexByteString
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
