{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Monad (
  Network
, runNetwork
, getSocket -- Internals?
, getConfig
, getUniverse
, readPacket
, readPacket'
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

import qualified Codec.Compression.Zlib as ZLib

import Control.Exception

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict as StateT
import Control.Monad.Base
import Control.Monad.Trans.Control

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import FlatParse.Basic as FP

import Network.Exception

import Network.Simple.TCP as Network

import Network.Protocol

import Network.Util.Builder
import Network.Util.VarNum

import Server.Monad (ServerM, Universe, Config)
import Server.Monad qualified as Server

import Util.Binary (FromBinary, ToBinary)
import qualified Util.Binary as Binary

newtype Network a = Network { unNetwork :: ServerM (ReaderT Socket (StateT ByteString IO)) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadBaseControl IO)

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
readPacket :: forall a . (Show a, FromBinary a) => Protocol -> Network a
readPacket = Network . lift . readPacket'

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
    OK !p !r -> liftIO . throwIO $ FailedExtraBytes (Showable p) r
    _ -> liftIO . throwIO $ FailedParse packetBs
  where
    withCompression (Protocol NoCompression  _) bs = Right bs
    withCompression (Protocol (Threshold _) _) bs =
      case runParser (Binary.get @VarInt) bs of
        OK 0 rem' -> Right rem'
        OK _ rem' -> Right . LBS.toStrict . ZLib.decompress $ LBS.fromStrict rem' -- TODO Check if I need the lazy bytestring...
        _         -> Left $ FailedDecompress bs
{-# INLINE readPacket #-}

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
sendPackets :: ToBinary a => Protocol -> Int -> [a] -> Network ()
sendPackets prot szEstimate as = sendBytes (LBS.fromChunks $ fmap (\a -> toStrictSizePrefixedByteString prot szEstimate $ Binary.put a) as)
{-# INLINE sendPackets #-}

closeConnection :: Network a
closeConnection = liftIO $ throwIO ConnectionClosed

invalidProtocol :: InvalidProtocol -> Network a
invalidProtocol = liftIO . throwIO
