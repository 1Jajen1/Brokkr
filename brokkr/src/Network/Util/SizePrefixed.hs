{-# LANGUAGE OverloadedStrings #-}
module Network.Util.SizePrefixed (
  ByteSizePrefixed(..)
) where
  
import Data.ByteString qualified as BS

import Data.Text          qualified as T
import Data.Text.Encoding qualified as T

import FlatParse.Basic as FP

import Mason.Builder qualified as B

import Network.Util.VarNum

import Util.Binary

newtype ByteSizePrefixed a = ByteSizePrefixed a

instance FromBinary (ByteSizePrefixed T.Text) where
  get = do
    VarInt len <- get
    bts <- FP.take $ fromIntegral len
    pure . ByteSizePrefixed $ T.decodeUtf8 bts
  {-# INLINE get #-}

instance ToBinary (ByteSizePrefixed T.Text) where
  put (ByteSizePrefixed t) =
    let bs = T.encodeUtf8 t
        sz = BS.length bs
    in put (VarInt $ fromIntegral sz) <> B.byteString bs
  {-# INLINE put #-}
