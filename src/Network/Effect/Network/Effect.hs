{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Effect.Network.Effect (
  Network(..)
, receiveBytes
, sendBytes
) where

import Effectful
import Data.ByteString
import Effectful.Dispatch.Dynamic
import Data.Primitive.SmallArray

-- This will most likely evolve when try stuff with io_uring and such, so don't depend on it too much and wrap it in Network.Effect.Packet
data Network :: Effect where
  ReceiveBytes :: Network m ByteString
  SendBytes :: SmallArray ByteString -> Network m ()

type instance DispatchOf Network = 'Dynamic

receiveBytes :: Network :> es => Eff es ByteString
receiveBytes = send ReceiveBytes

sendBytes :: Network :> es => SmallArray ByteString -> Eff es ()
sendBytes = send . SendBytes
