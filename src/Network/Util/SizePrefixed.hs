{-# LANGUAGE OverloadedStrings #-}
module Network.Util.SizePrefixed (
  ByteSizePrefixed(..)
) where
import Util.Binary
import Network.Util.VarNum
import FlatParse.Basic
import Util.Flatparse

newtype ByteSizePrefixed a = ByteSizePrefixed a

instance FromBinary a => FromBinary (ByteSizePrefixed a) where
  get = do
    VarInt len <- get
    bts <- takeN $ fromIntegral len
    case runParser (get @a) bts of
      OK a "" -> return $ ByteSizePrefixed a
      _ -> empty
  {-# INLINE get #-}
