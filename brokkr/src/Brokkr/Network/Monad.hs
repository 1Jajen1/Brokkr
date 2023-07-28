{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Network.Monad (
  Network
, runNetwork
, getSocket -- Internals?
, getConfig
, getUniverse
, readPacket
, sendBytes
, sendPackets
, closeConnection
, ConnectionClosed(..)
, PacketFailure(..)
, invalidProtocol
, InvalidProtocol(..)
, ClientTimedOut(..)
, InvalidKeepAlive(..)
) where

import Brokkr.Debug.Monad
import Brokkr.Network.Exception

import Brokkr.Packet.Decode qualified as Decode
import Brokkr.Packet.Encode qualified as Encode

import Brokkr.Server.Monad (ServerM, Universe, Config)
import Brokkr.Server.Monad qualified as Server

import Control.Exception

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict as StateT
import Control.Monad.Base
import Control.Monad.Trans.Control

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

import Data.Proxy

import GHC.TypeLits

import Network.Simple.TCP as Network

newtype Network a = Network { _unNetwork :: ServerM (ReaderT Socket (StateT ByteString IO)) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadBaseControl IO, MonadTrace)

deriving newtype instance Server.MonadHecs Universe Network

runNetwork :: Config -> Universe -> Socket -> Network a -> IO a
-- We defer f because that means another thread (the main thread) will sync all changes, this ensures thread safety
-- This does not affect reads, and packets are not written as components, but put into a queue
-- This would technically sync after f finishes, but the network threads are designed to diverge. This means no thread running Network should
-- finish without an exception
runNetwork conf universe sock (Network f) = catch
  (evalStateT (runReaderT (Server.runServerM conf universe (Server.defer f)) sock) mempty)
  (\(e :: SomeException) -> do
    -- print e
    throwIO e
    )
{-# INLINE runNetwork #-}

getConfig :: Network Config
getConfig = Network Server.getConfig

getUniverse :: Network Universe
getUniverse = Network Server.getUniverse
{-# INLINE getUniverse #-}

getSocket :: Network Socket
getSocket = Network $ lift ask
{-# INLINE getSocket #-}

-- Test code would like just the network part not wrapped in 'ServerM', so rewrap it here
readPacket :: forall compression a . (Show a, Decode.FromBinary a, Decode.KnownCompression compression)
  => Proxy compression -> Decode.CompressionSettings -> Decode.EncryptionSettings -> Network a
{-# SPECIALIZE readPacket :: forall a . (Show a, Decode.FromBinary a)
  => Proxy Decode.NoCompression -> Decode.CompressionSettings -> Decode.EncryptionSettings -> Network a #-}
{-# SPECIALIZE readPacket :: forall a sz . (Show a, Decode.FromBinary a, KnownNat sz)
  => Proxy (Decode.UseCompression sz) -> Decode.CompressionSettings -> Decode.EncryptionSettings -> Network a #-}
{-# SPECIALIZE readPacket :: forall a . (Show a, Decode.FromBinary a)
  => Proxy Decode.SomeCompression -> Decode.CompressionSettings -> Decode.EncryptionSettings -> Network a #-}
readPacket p cs es = do
  sock <- Network $ lift ask
  leftover <- Network . lift $ lift get
  st <- liftBaseWith $ \runInBase -> 
    Decode.readPacket @_ @a p cs es
      (Network.recv sock 1024 >>= \case Nothing -> throwIO ConnectionClosed; Just x -> pure x)
      leftover
      $ \a leftover' -> do
        runInBase $ do
          Network . lift . lift $ put leftover'
          pure a
  restoreM st

{-
-- TODO Move Protocol to Type level
-- We can introduce it from some config and specialize for all protocol and packet types. That should remove all abstraction cost and only
-- leave my bad code to blame for performance problems.
readPacket' :: forall a . (Show a, FromBinary a) => Protocol -> ReaderT Socket (StateT ByteString IO) a
readPacket' prot = do
  -- TODO Binary.get @VarInt >>= will always allocate. Use CPS to avoid allocating.
  -- ^- This seems to be true for most loops in the parser...
  -- The ByteString will also be allocated, not good!
  !packetBs <- receiveBytes $ \bs -> case runParser (Binary.get @VarInt >>= FP.take . fromIntegral) bs of
    OK res rem' -> case withCompression prot res of
      Right res' -> Done res' rem'
      Left e     -> Failed e
    Fail         -> NeedMore
  case runParser (Binary.get @a) packetBs of
    OK !p !r | BS.length r == 0 -> pure p
    OK !p !r -> liftIO . throwIO . FailedExtraBytes (Showable p) $ HexBS r
    _ -> liftIO . throwIO . FailedParse $ HexBS packetBs
  where
    withCompression (Protocol NoCompression  _) bs = Right bs
    withCompression (Protocol (Threshold _) _) bs =
      case runParser (Binary.get @VarInt) bs of
        OK 0 rem' -> Right rem'
        -- TODO Check that we have the correct size
        OK _ rem' -> Right . LBS.toStrict . ZLib.decompress $ LBS.fromStrict rem' -- TODO Check if I need the lazy bytestring...
        _         -> Left . FailedDecompress $ HexBS bs
{-# INLINE readPacket' #-}

data StreamParse =
    Done {-# UNPACK #-} !ByteString {-# UNPACK #-} !ByteString
  | NeedMore
  | forall e . Exception e => Failed e

receiveBytes :: (ByteString -> StreamParse) -> ReaderT Socket (StateT ByteString IO) ByteString
receiveBytes test = do
  sock <- ask
  bs <- lift StateT.get
  go sock bs
  where
    go sock bs = case test bs of
      Done res rem' -> lift $ put rem' >> pure res
      NeedMore -> Network.recv sock 1024 >>= \case
        Just new -> go sock (bs <> new)
        Nothing -> liftIO $ throwIO ConnectionClosed
      Failed e -> liftIO $ throwIO e
{-# INLINE receiveBytes #-}

-}

sendBytes :: LBS.ByteString -> Network ()
sendBytes bs = Network $ do
  sock <- lift ask
  liftIO $ Network.sendLazy sock bs
{-# INLINE sendBytes #-}

-- TODO: Check if this sufficiently inlines to avoid intermediate lists when
  -- a) giving it fixed size lists
  -- b) passing it lists created from vector slices
-- TODO: Also it would be great if this is strict enough to not have any lazy refs after this is done, so we can use unsafe vector slices (no copies)
-- If all else fails use TH?
-- sendLazy alone will always expect a lazy bytestring so probably not worth it?
-- TODO This needs work!
sendPackets :: (Encode.ToBinary a, Encode.KnownCompression compression) => Proxy compression -> Encode.CompressionSettings -> Encode.EncryptionSettings -> [Encode.Packet a] -> Network ()
sendPackets p cs es as = sendBytes
  (LBS.fromChunks $ fmap (Encode.toStrictByteString p cs es) as)
{-# INLINE sendPackets #-}

closeConnection :: Network a
closeConnection = liftIO $ throwIO ConnectionClosed

invalidProtocol :: InvalidProtocol -> Network a
invalidProtocol = liftIO . throwIO
