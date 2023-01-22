module Network.Packet.Server.Play.PlayerCommand (
  PlayerCommand(..)
) where

import Data.Functor
import Data.Word

import Network.Util

import Util.Binary

data PlayerCommand =
    StartSneak
  | StopSneak
  | LeaveBed
  | StartSprint
  | StopSprint
  | StartHorseJump HorseJumpBoost
  | StopHorseJump
  | OpenHorseInventory
  | StartFlyingWithElytra
  deriving stock (Show, Eq)

instance FromBinary PlayerCommand where
  get = get @VarInt >>= \case
    0 -> get @Word8 $> StartSneak
    1 -> get @Word8 $> StopSneak
    2 -> get @Word8 $> LeaveBed
    3 -> get @Word8 $> StartSprint
    4 -> get @Word8 $> StopSprint
    5 -> StartHorseJump <$> get
    6 -> get @Word8 $> StopHorseJump
    7 -> get @Word8 $> OpenHorseInventory
    8 -> get @Word8 $> StartFlyingWithElytra

newtype HorseJumpBoost = HorseJumpBoost Int
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via FromIntegral Int VarInt
