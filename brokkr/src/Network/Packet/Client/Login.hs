{-# LANGUAGE TemplateHaskell #-}
module Network.Packet.Client.Login (
  LoginPacket(..)
) where

import Util.UUID
import Data.Text
import Util.Binary
import Network.Util.Packet
import Network.Util.MCString
import Network.Util.VarNum

-- TODO
import Data.Coerce
import FlatParse.Basic qualified as FP

data LoginPacket =
    Disconnect
  | EncryptionRequest
  | LoginSuccess UUID Text -- Newtype to properly control size (16 bytes)
  | SetCompression Int
  deriving stock Show

instance ToBinary LoginPacket where
  put a = packetId a <> case a of
    LoginSuccess uid name -> put uid <> put (MCString name) <> put (VarInt 0) -- TODO properties
    SetCompression i -> put (VarInt $ fromIntegral i)
    _ -> error "Unsupported"
  {-# INLINE put #-}

instance FromBinary LoginPacket where
  get = $$(mkPacketParser [
      [|| pure Disconnect ||]
    , [|| pure EncryptionRequest ||]
    , [|| do
        res <- LoginSuccess <$> get <*> (coerce <$> get @MCString)
        FP.skip 1 -- TODO properties
        pure res
        ||]
    , [|| SetCompression . fromIntegral <$> get @VarInt ||]
    ])

{- Note: Max byte sizes
  Disconnect is bounded by MCString which has a max size of (327676 * 4) + 3, so for all intents and purposes it is unbounded.
  LoginSuccess is 16 bytes + (16 * 4) + 1 = 81 bytes
                   ^            ^       ^ 
                 UUID        string   str prefix (VarInt for numbers < 127 will only need 1 byte)
    - A more sensible number tho would probably be to assume ascii chars and thus 16 + (16 * 1) + 1 = 33 bytes
  SetCompression is 5 bytes, but usually will be 1-2 bytes as thresholds higher than 2^14 make little sense imo (mc default is 256, so 2 bytes for the VarInt)
                    ^
                 Threshold
  
  So the sensible max (ignoring Disconnect) is 33 bytes. Next adding the packet id (1 byte) and two length prefixes (both 1 byte assuming compression will only ever lower size)
   we get 36 bytes. So 64 Byte buffers should satisfy most requests and double that will satisfy all requests (ignoring Disconnect).
  
  Disconnect will probably also not sent crazy long messages so starting by 64 and doubling will work for almost all cases.

  Conclusion: If I ever use a custom strategy to run these builders, that should happen on 64 Byte buffers.
  => masons default for strict bytestrings is 128 so for now not too bad either.
-}

