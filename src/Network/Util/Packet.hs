{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Util.Packet (
  packetId
, mkPacketParser
) where

import qualified Mason.Builder as B

import GHC.Exts (dataToTag#, Int(I#))
import Network.Util.VarNum
import Util.Binary
import Language.Haskell.TH
import FlatParse.Basic
import Data.Void

packetId :: a -> B.Builder
packetId !a = put $ VarInt $ fromIntegral (I# (dataToTag# a))

mkPacketParser :: [Code Q (Parser Void a)] -> Code Q (Parser Void a)
mkPacketParser [] = [|| empty ||]
mkPacketParser subs = [||
  do
    VarInt pid <- get @VarInt
    $$(unsafeCodeCoerce $ caseE [| pid |] $ (
      do
        (i, p) <- zip [0..] subs
        pure $ match (litP $ IntegerL i) (normalB $ unTypeCode p) []
      ) <> [match wildP (normalB [| error $ "Unknown packet with id: " <> show pid |]) []])
  ||]
