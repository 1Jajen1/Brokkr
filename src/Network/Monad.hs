{-# LANGUAGE LinearTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Monad (
  MonadNetwork(..)
, NetworkM
, runNetwork
, readPacket
, sendPacket
, sendPackets
) where

import Data.ByteString
import qualified Data.ByteString.Lazy as LBS
import Network.Simple.TCP
import qualified Network.Simple.TCP as Network
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Trans
import qualified Util.Binary as Binary
import Network.Util.Builder
import Util.Binary (ToBinary, FromBinary)
import Network.Util.VarNum
import FlatParse.Basic
import Entity.Id.Monad ( MonadEntityId )
import Util.Lift
import Util.Log ( MonadLog )
import Util.Time ( MonadTime )
import Game.State (MonadGameState)
import Network.Protocol
import qualified Codec.Compression.Zlib as ZLib
import Util.Exception

class Monad m => MonadNetwork m where
  receiveBytes :: (ByteString -> Maybe (ByteString, ByteString)) -> m ByteString
  sendBytes    :: LBS.ByteString -> m ()

newtype NetworkM m a = NetworkM { runNetworkM :: ReaderT Socket (StateT ByteString m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadEntityId, MonadLog, MonadGameState)
  deriving MonadTime via (Lift NetworkM m) 

deriving via (Lift (ReaderT r) m) instance MonadNetwork m => MonadNetwork (ReaderT r m) 
deriving via (Lift (StateT s) m) instance MonadNetwork m => MonadNetwork (StateT s m) 

instance (Monad (t m), MonadTrans t, MonadNetwork m) => MonadNetwork (Lift t m) where
  receiveBytes test = Lift . lift $ receiveBytes test
  {-# INLINE receiveBytes #-}
  sendBytes bs = Lift . lift $ sendBytes bs
  {-# INLINE sendBytes #-}

runNetwork :: Monad m => Socket -> NetworkM m a -> m a
runNetwork sock = flip evalStateT mempty . flip runReaderT sock . runNetworkM
{-# INLINE runNetwork #-}

instance MonadTrans NetworkM where
  lift = NetworkM . lift . lift
  {-# INLINE lift #-}

instance MonadBracket m => MonadBracket (NetworkM m) where 
  bracket acq clean act = do
    sock <- NetworkM ask
    st <- NetworkM $ lift $ get
    (ret, st') <- lift $ bracket (runNetwork sock acq) (\a exc -> runNetwork sock $ clean a exc) $ flip runStateT st . flip runReaderT sock . runNetworkM . act
    NetworkM $ lift $ put st'
    pure ret
  {-# INLINE bracket #-}

instance MonadIO m => MonadNetwork (NetworkM m) where
  receiveBytes test = NetworkM $ do
    sock <- ask
    bs <- lift get
    go sock bs
    where
      go sock bs = case test bs of
        Just (!res, !rem') -> lift $ put rem' >> pure res
        Nothing -> Network.recv sock 1024 >>= \case
          Just new -> go sock (bs <> new)
          Nothing -> error "TODO Conn closed"
  {-# INLINE receiveBytes #-} -- ^^ TODO This can probably be better ...
  sendBytes bs = NetworkM $ do
    sock <- ask
    liftIO $ Network.sendLazy sock bs
  {-# INLINE sendBytes #-}

readPacket :: forall a m . (MonadNetwork m, FromBinary a) => Protocol -> m a
readPacket prot = do
  !packetBs <- receiveBytes $ \bs -> case runParser (Binary.get @VarInt >>= takeBs . fromIntegral) bs of
    OK res rem' -> Just (withCompression prot res, rem')
    Fail       -> Nothing
  case runParser (Binary.get @a) packetBs of
    OK !p _ -> pure p -- TODO Decide wether or not the remaining bytes should be empty...
    _ -> error $ "Failed to parse packet\n" <> show packetBs -- TODO
  where
    withCompression (Protocol NoCompression  _) bs = bs
    withCompression (Protocol (Threshold _) _) bs =
      case runParser (Binary.get @VarInt) bs of
        OK 0 rem' -> rem'
        OK _ rem' -> LBS.toStrict . ZLib.decompress $ LBS.fromStrict rem'
        _ -> error $ "Failed to decompress packet " <> show bs 
{-# INLINE readPacket #-}

sendPacket :: (ToBinary a, MonadNetwork m) => Protocol -> Int -> a -> m ()
sendPacket prot szEstimate a = sendBytes (LBS.fromStrict $ toStrictSizePrefixedByteString prot szEstimate (Binary.put a))
{-# INLINE sendPacket #-}

sendPackets :: (ToBinary a, MonadNetwork m) => Protocol -> Int -> [a] -> m ()
sendPackets prot szEstimate as = sendBytes (LBS.fromChunks $ fmap (\a -> toStrictSizePrefixedByteString prot szEstimate $ Binary.put a) as)
{-# INLINE sendPackets #-}

