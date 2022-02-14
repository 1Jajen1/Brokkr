{-# LANGUAGE ImpredicativeTypes #-}
module Network.Utils (
  encodePackets
) where

import qualified Data.ByteString as BS
import Network.Effect.Packet (toStrictSizePrefixedByteString)
import qualified Mason.Builder as B

encodePackets :: [[B.Builder]] -> [BS.ByteString]
encodePackets toSend = foldMap (\x -> toStrictSizePrefixedByteString 128 x) <$> toSend
