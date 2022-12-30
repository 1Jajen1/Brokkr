{-# LANGUAGE TemplateHaskell #-}
module Network.Packet.Server.Login (
  LoginPacket(..)
) where

import FlatParse.Basic
import Data.UUID
import Data.Text hiding (empty)
import Data.Int
import Data.Word
import Data.Coerce
import Util.Binary
import Network.Util.Packet
import Network.Util.MCString
import Network.Util.VarNum
import qualified Data.Vector.Storable as S

import Debug.Trace

data LoginPacket =
    LoginStart Text SignatureData (Maybe UUID)
  deriving stock Show

data SignatureData = NoSignatureData
  deriving stock Show

instance FromBinary LoginPacket where
  get = $$(mkPacketParser [
      [|| loginStart ||]
    ])
    where
      loginStart = do
        uName <- coerce <$> get @MCString
        signatureData <- get >>= \case
          True -> do
            get @Int64
            get @(SizePrefixed VarInt (S.Vector Word8))
            get @(SizePrefixed VarInt (S.Vector Word8))
            pure NoSignatureData -- TODO
          False -> pure NoSignatureData
        uuid <- get >>= \case
          True -> Just <$> get
          False -> pure Nothing
        pure $ LoginStart uName signatureData uuid
