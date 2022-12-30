module Command (
  Command(..)
, handle
) where

import Command.Handler

import qualified Command.Handler.Disconnect as Disconnect
import qualified Command.Handler.Login as Login
import qualified Command.Handler.Movement as Move

import Client

import Util.Position
import Util.Rotation

data Command =
    JoinClient
  | DisconnectClient -- TODO Add reason
  | MoveAndRotateClient Position Rotation OnGround
  | RotateClient Rotation OnGround
  | MoveClient Position OnGround
  | SetOnGround OnGround
  deriving stock Show

handle :: Client -> Command -> Handler ()

handle p JoinClient = Login.handle p

handle p DisconnectClient = Disconnect.handle p

handle p (MoveAndRotateClient pos rot onGround) = Move.handleMove p pos >> Move.handleTurn p rot >> Move.handleOnGround p onGround
handle p (RotateClient rot onGround) = Move.handleTurn p rot >> Move.handleOnGround p onGround
handle p (MoveClient pos onGround) = Move.handleMove p pos >> Move.handleOnGround p onGround
handle p (SetOnGround onGround) = Move.handleOnGround p onGround

{-

Builder and parser work:

Builder:
  - We usually know either statically or right before building the amount of memory we will need, only very few types (NBT) are annoying to calculate before
  - Thus change the builder to avoid checking if we have enough space
    - ie writing (Int, Int, NBT, Int, Int) should first check for 16 bytes, then write the NBT while checking dynamically, and then check for 32 bytes
      - (Int, Int, Int, VarInt, Int) should check for 24 + 5 + 8 = 37 bytes at the start and just write
  - Like this should already be happening but for some reason the `ensure` rewrite rule won't fire

Parser:
  - Write own lib. Have fixed size parse fuse (so parsing anything non-looping fuses) and try to get rid of more allocations
    - backtracking won't be needed. NBT always prefixes the type so we never miss a branch (in packet parsing that is, rest probably won't need it either)
  - Maybe CPS style: FPContents -> Addr# -> Addr# -> ((# a, Addr# #) -> r) -> r -> r
                                    end      curr      cont                 fail
    - then fuse size bounds with: ensure :: Int# -> (Addr# -> a) -> Parser a
      - This means the function always has exactly the argument amount of bytes and the parser will also advance that amount
      ensure a f >>= \r -> ensure b (g r) = ensure (a + b) $ \addr -> g (f addr) $ addr + b or smth

-}
