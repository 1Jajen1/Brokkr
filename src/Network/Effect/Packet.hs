{-# LANGUAGE OverloadedStrings #-}
module Network.Effect.Packet (
  readPacket
) where
  
import Effectful.State.Static.Local
import Data.ByteString
import Network.Effect.Network
import Effectful
import Data.Void
import FlatParse.Basic
import qualified Util.Binary as Binary
import Network.Util.VarNum
import Util.Flatparse

readPacket ::
  ( State ByteString :> es
  , Network :> es)
  => Parser Void a
  -> Eff es a
readPacket parser = do
  leftover <- get
  (packetBs, leftoverBs) <- go leftover
  put leftoverBs
  case runParser parser packetBs of
    OK p "" -> pure p
    _ -> error "Failed to parse packet" -- TODO
  where
    lenParser = do
      VarInt len <- Binary.get @VarInt
      takeN $ fromIntegral len
    go bs = do
      case runParser lenParser bs of
        OK resBs remBs -> pure (resBs, remBs)
        Fail -> do
          moreBs <- receiveBytes
          go (bs <> moreBs)
