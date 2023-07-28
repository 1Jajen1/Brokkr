{-# LANGUAGE DataKinds #-}
module Brokkr.Packet.ServerToClient.Login.Types.Internal (
  ServerId(..), mkServerId
, Property(..), mkProperty
, CompressionThreshold(..)
, RequestData(..)
) where

import Brokkr.Packet.Binary
import Brokkr.Packet.MCString

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Int
import Data.Text.Internal (Text(..))

import FlatParse.Basic qualified as Flatparse

newtype ServerId = UnsafeServerId Text
  deriving stock Show
  deriving (ToBinary, FromBinary) via MCString 20

mkServerId :: Text -> Maybe ServerId
mkServerId = coerce . mkMCString @20

data Property = UnsafeProperty {
  propertyName      :: !Text
, propertyValue     :: !Text
, propertySignature :: !(Maybe Text) -- TODO Is this correct? This is from wiki.vg, but I'd assume this is just bytes?
}
  deriving stock Show

instance ToBinary Property where
  put (UnsafeProperty name val msig) = put (UnsafeMCString name) <> put (UnsafeMCString val) <> case msig of
    Nothing -> put False
    Just sig -> put True <> put (UnsafeMCString sig)
  {-# INLINE put #-}

instance FromBinary Property where
  with f =
    with $ \(UnsafeMCString @32767 name) ->
      with $ \(UnsafeMCString @32767 val) ->
        with $ \case
          False -> f (UnsafeProperty name val Nothing)
          True -> with $ \(UnsafeMCString @32767 sig) ->
            f (UnsafeProperty name val (Just sig))
  {-# INLINE with #-}

mkProperty :: Text -> Text -> Maybe Text -> Maybe Property
mkProperty name value sig
  | textLen name < 32767 && textLen value < 32767 && maybe True (\t -> textLen t < 32767) sig = Just $ UnsafeProperty name value sig
  | otherwise = Nothing

textLen :: Text -> Int
textLen (Text _ _ tLen) = tLen

newtype CompressionThreshold = CompressionThreshold Int32
  deriving stock Show
  deriving (ToBinary, FromBinary) via VarInt

newtype RequestData = RequestData ByteString
  deriving newtype ToBinary

instance FromBinary RequestData where
  get = RequestData <$> Flatparse.takeRest
  {-# INLINE get #-}

instance Show RequestData where
  showsPrec :: Int -> RequestData -> ShowS
  showsPrec prec (RequestData bs) = showParen (prec > 10) $
    showString "RequestData " . showsPrec 11 (HexBS bs)

