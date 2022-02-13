{-# LANGUAGE OverloadedStrings #-}
module Network.Util.SizePrefixed (
  ByteSizePrefixed(..)
) where
import Util.Binary
import Network.Util.VarNum
import FlatParse.Basic
import Util.Flatparse
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Mason.Builder as B

newtype ByteSizePrefixed a = ByteSizePrefixed a

instance FromBinary a => FromBinary (ByteSizePrefixed a) where
  get = do
    VarInt len <- get
    bts <- takeN $ fromIntegral len
    case runParser (get @a) bts of
      OK a "" -> return $ ByteSizePrefixed a
      _ -> empty
  {-# INLINE get #-}

instance ToBinary (ByteSizePrefixed T.Text) where
  put (ByteSizePrefixed t) =
    let bs = T.encodeUtf8 t
        sz = BS.length bs
    in put (VarInt $ fromIntegral sz) <> B.byteString bs
  {-# INLINE put #-}
