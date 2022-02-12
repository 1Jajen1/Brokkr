{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Effect.Network.Effect (
  Network(..)
, receiveBytes
) where

import Effectful
import Data.ByteString
import Effectful.Dispatch.Dynamic

data Network :: Effect where
  ReceiveBytes :: Network m ByteString

type instance DispatchOf Network = 'Dynamic

receiveBytes :: Network :> es => Eff es ByteString
receiveBytes = send ReceiveBytes

