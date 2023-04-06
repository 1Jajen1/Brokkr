module Utils.Fault (
  WithFault(..)
, GenFault(..)
, AnyBytes(..)
) where

import Data.ByteString (ByteString)

import qualified Data.ByteString as BS

import Hedgehog (Gen)
import qualified Mason.Builder as M
import Util.Binary
import qualified FlatParse.Basic as FP

-- TODO This needs more work
-- it needs to be packet specific:
-- class Fault a where
--   genFault :: Gen (WithFault a)
-- newtype WithFault a = WithFault ByteString
-- instance ToBinary (WithFault a) where put (WithFault bs) = byteString bs
--
-- A fault would be anything that causes the client to be disconnected, so any invalid packet
-- Packets that are just split or have extra bytes may still be valid, same with reordered packets

data Fault =
    NoFault
  deriving stock Show

newtype WithFault a = WithFault ByteString

instance ToBinary (WithFault a) where
  put (WithFault bs) = M.byteString bs

class GenFault a where
  genFault :: Gen (WithFault a)

newtype AnyBytes = AnyBytes ByteString

instance Show AnyBytes where
  show (AnyBytes bs) = show $ BS.unpack bs

instance FromBinary AnyBytes where
  get = AnyBytes <$> FP.takeRest

