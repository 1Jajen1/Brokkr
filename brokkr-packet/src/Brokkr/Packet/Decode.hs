{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveAnyClass #-}
module Brokkr.Packet.Decode (
  readPacket
, FromBinary
, module Brokkr.Packet.Settings
) where

import Brokkr.Packet.Binary
import Brokkr.Packet.Settings

import Brokkr.VarInt.Decode

import Control.Exception (Exception)

import qualified Codec.Compression.Zlib as ZLib

import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Data.Proxy

import FlatParse.Basic qualified as Flatparse

import GHC.TypeLits (KnownNat)

import GHC.Exts
import GHC.ForeignPtr
import GHC.IO

-- TODO This is a bit ugly. The reason for that is that Flatparse cannot easily inline as much as I want to.
-- They can't really do that generically, but in this specific case it should be safe, so I manually unwrapped the parsers
-- TODO Double check that touch# works fine here
--  It should be fine because the different continuations are not diverging and touch# will always be evaluated before
--  code that throws or continues with the user continuation 
readPacket :: forall compression a r .
  (Show a, FromBinary a, KnownCompression compression)
  => Proxy compression -> CompressionSettings -> EncryptionSettings -> IO ByteString -> ByteString -> (a -> ByteString -> IO r) -> IO r
readPacket cs compressionSettings0 _encryptionSettings more leftover0 cont = do
  let compressionSettings = compressionVal cs compressionSettings0
      -- Use the low level varint decoding primitives to avoid allocating
      headerParser eob s st =
        case compressionSettings of
          NoCompression -> case withVarInt# @5 eob s st of
            (# st', (# | | (# e #) #) #) -> (# st', (# | | (# VarIntError e #) #) #)
            (# st', (# | (# #) | #) #) -> (# st', (# | (# #) | #) #)
            (# st', (# (# l, s' #) | | #) #) -> (# st', (# (# l, 0#, s' #) | | #) #)
          UseCompression _ -> case withVarInt# @5 eob s st of
            (# st', (# | | (# e #) #) #) -> (# st', (# | | (# VarIntError e #) #) #)
            (# st', (# | (# #) | #) #) -> (# st', (# | (# #) | #) #)
            (# st', (# (# l, s' #) | | #) #) ->
              case withVarInt# @5 eob s' st' of
                (# st'', (# | | (# e #) #) #) -> (# st'', (# | | (# VarIntError e #) #) #)
                (# st'', (# | (# #) | #) #) -> (# st'', (# | (# #) | #) #)
                (# st'', (# (# r, s'' #) | | #) #) -> (# st'', (# (# l -# 1#, r, s'' #) | | #) #)
      goStreaming inputBs@(BS.BS (ForeignPtr ptr fp) (I# len)) =
          let endPtr = plusAddr# ptr len
          in IO $ \s -> case headerParser endPtr ptr s of
            -- Throw the error
            (# s', (# | | (# e #) #) #) -> case throwIO e of IO f -> f (touch# fp s')
            -- Rerun the parser with more input
            (# s', (# | _ | #) #) ->
              -- TODO (<>) allocates the BS constructor. Copy manually?
              case more >>= \bs -> goStreaming (inputBs <> bs) of IO f -> f (touch# fp s')
            -- Run the packet parser
            (# s', (# (# sz#, packSz#, ptr' #) | | #) #) ->
              if | I# sz# <= 0 -> case throwIO $ ReadPacketError (InvalidPacketSize (I# sz#)) (HexBS inputBs) of IO f -> f (touch# fp s')
                 -- Check that we have enough bytes to continue
                 | isTrue# (sz# ># minusAddr# endPtr ptr') -> case more >>= \bs -> goStreaming (inputBs <> bs) of IO f -> f (touch# fp s')
                 -- Do we use compression?
                 | UseCompression threshold <- compressionSettings
                 , I# packSz# /= 0
                   -> if I# packSz# < fromIntegral threshold
                     -- If we have compressed data but it should have been uncompressed, fail!
                     then case throwIO (InvalidCompressedPacketThreshold (I# packSz#) (fromIntegral threshold)) of IO f -> f (touch# fp s')
                     else
                       let !(BS.BS (ForeignPtr startPtr fp') (I# len#)) = LBS.toStrict . ZLib.decompress $ LBS.fromStrict (BS.BS (ForeignPtr ptr' fp) (I# sz#))
                       in if isTrue# (packSz# /=# len)
                         then case throwIO (InvalidCompressedPacketSize (I# packSz#) (I# len)) of IO f -> f (touch# fp s')
                         else let leftoverPtr = plusAddr# ptr' sz#
                              in  parsePacket (BS.BS (ForeignPtr leftoverPtr fp) (I# (minusAddr# endPtr leftoverPtr))) fp' startPtr len# s'
                 -- No compression
                 -- No need for touch# here as the leftover and fp are pointing to the same object and
                 -- parsePacket will keep fp alive
                 | otherwise -> let leftoverPtr = plusAddr# ptr' sz#
                                in parsePacket (BS.BS (ForeignPtr leftoverPtr fp) (I# (minusAddr# endPtr leftoverPtr))) fp ptr' sz# s'
      parsePacket !leftoverBs fp startPtr sz s =
        let endPtr' = plusAddr# startPtr sz
        in case Flatparse.runParserT# (get @a) fp endPtr' startPtr s of
          Flatparse.Err# s'' e ->
            case throwReadError e (BS.BS (ForeignPtr startPtr fp) (I# sz)) of IO f -> f (touch# fp s'')
          Flatparse.Fail# s'' ->
            case throwIO (FailedGeneric . HexBS $ BS.BS (ForeignPtr startPtr fp) (I# (minusAddr# endPtr' startPtr))) of IO f -> f (touch# fp s'')
          Flatparse.OK# s'' res finPtr
            | isTrue# (eqAddr# finPtr endPtr') ->
              case cont res leftoverBs of IO f -> f (touch# fp s'') -- We still need to touch here because there is no guaranteed that f keeps fp alive
            | otherwise -> 
              case throwIO (FailedExtraBytes (Showable res) . HexBS $ BS.BS (ForeignPtr finPtr fp) (I# (minusAddr# endPtr' finPtr))) of IO f -> f (touch# fp s'')
  goStreaming leftover0
{-# SPECIALIZE INLINE readPacket ::
  forall a r . (Show a, FromBinary a)
    => Proxy NoCompression -> CompressionSettings -> EncryptionSettings -> IO ByteString -> ByteString -> (a -> ByteString -> IO r) -> IO r #-}
{-# SPECIALIZE INLINE readPacket ::
  forall threshold a r . (Show a, FromBinary a, KnownNat threshold)
    => Proxy (UseCompression threshold) -> CompressionSettings -> EncryptionSettings -> IO ByteString -> ByteString -> (a -> ByteString -> IO r) -> IO r #-}
{-# SPECIALIZE INLINE readPacket ::
  forall a r . (Show a, FromBinary a)
    => Proxy SomeCompression -> CompressionSettings -> EncryptionSettings -> IO ByteString -> ByteString -> (a -> ByteString -> IO r) -> IO r #-}

throwReadError :: PacketParseError -> ByteString -> IO a
throwReadError e bs = throwIO $ ReadPacketError e (HexBS bs) 

data ReadPacketError = ReadPacketError !PacketParseError !HexByteString
  deriving stock Show
  deriving anyclass Exception
