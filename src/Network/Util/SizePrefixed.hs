{-# LANGUAGE OverloadedStrings #-}
module Network.Util.SizePrefixed (
  ByteSizePrefixed(..)
) where
import Util.Binary
import Network.Util.VarNum
import Util.Flatparse
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Mason.Builder as B

newtype ByteSizePrefixed a = ByteSizePrefixed a

instance FromBinary (ByteSizePrefixed T.Text) where
  get = do
    VarInt len <- get
    bts <- takeN $ fromIntegral len
    pure . ByteSizePrefixed $ T.decodeUtf8 bts
  {-# INLINE get #-}

instance ToBinary (ByteSizePrefixed T.Text) where
  put (ByteSizePrefixed t) =
    let bs = T.encodeUtf8 t
        sz = BS.length bs
    in put (VarInt $ fromIntegral sz) <> B.byteString bs
  {-# INLINE put #-}
