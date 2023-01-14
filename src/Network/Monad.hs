{-# LANGUAGE PatternSynonyms #-}
module Network.Monad (
  Network
, runNetwork
, getSocket -- Internals?
, getServer
, readPacket
, sendBytes
, sendPackets
, withAsync
, closeConnection
, ConnectionClosed(..)
, PacketFailure(..)
, invalidProtocol
, InvalidProtocol(..)
, ClientTimedOut(..)
, InvalidKeepAlive(..)
) where

import qualified Codec.Compression.Zlib as ZLib

import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async

import Control.Exception

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import FlatParse.Basic

import Network.Exception

import Network.Simple.TCP as Network

import Network.Protocol

import Network.Util.Builder
import Network.Util.VarNum

import Server

import Util.Binary (FromBinary, ToBinary)
import qualified Util.Binary as Binary

import Debug.Trace

newtype Network a = Network { unNetwork :: ReaderT Server (ReaderT Socket (StateT ByteString IO)) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runNetwork :: Server -> Socket -> Network a -> IO a
runNetwork server sock (Network f) = evalStateT (runReaderT (runReaderT f server) sock) mempty
{-# INLINE runNetwork #-}

getServer :: Network Server
getServer = Network ask
{-# INLINE getServer #-}

getSocket :: Network Socket
getSocket = Network $ lift ask
{-# INLINE getSocket #-}

-- TODO Move Protocol to Type level
-- We can introduce it from some config and specialize for all protocol and packet types. That should remove all abstraction cost and only
-- leave my bad code to blame for perf.
readPacket :: forall a . (Show a, FromBinary a) => Protocol -> Network a
readPacket prot = do
  -- TODO Binary.get @VarInt >>= will always allocate. Use CPS to avoid allocating.
  -- ^- This seems to be true for most loops in the parser...
  -- The ByteString will also be allocated, not good!
  !packetBs <- receiveBytes $ \bs -> case runParser (Binary.get @VarInt >>= takeBs . fromIntegral) bs of
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

receiveBytes :: (ByteString -> StreamParse) -> Network ByteString
receiveBytes test = Network $ do
  sock <- lift ask
  bs <- lift $ lift get
  go sock bs
  where
    go sock bs = case test bs of
      Done res rem' -> lift . lift $ put rem' >> pure res
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

withAsync :: IO a -> (Async a -> Network b) -> Network b
withAsync inAsync f = Network $ do
  server <- ask
  sock <- lift ask
  bs <- lift $ lift get
  liftIO . Async.withAsync inAsync $ \as -> evalStateT (runReaderT (runReaderT (unNetwork $ f as) server) sock) bs
{-# INLINE withAsync #-}
