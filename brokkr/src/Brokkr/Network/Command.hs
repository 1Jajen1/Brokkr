module Brokkr.Network.Command (
  Command
) where

import Brokkr.Packet.ClientToServer.Play (PlayPacket)

type Command = PlayPacket

