{-# LANGUAGE MagicHash  #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UndecidableInstances #-}
module Entity.Id.Monad (
  MonadEntityId(..)
, EntityIdM
, runEntityId
) where

import Entity.Internal (EntityId(..))

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Primitive.ByteArray
import Control.Monad.Primitive
import GHC.Exts (Int(I#), fetchAddIntArray#)
import Control.Monad.Trans
import Data.Primitive
import Control.Monad.Trans.State.Strict
import Util.Lift
import Util.Time
import Control.Monad.IO.Unlift

class Monad m => MonadEntityId m where
  freshEntityId :: m EntityId

newtype EntityIdM m a = EntityIdM (ReaderT (EntityIdRef (PrimState m)) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad, MonadTime)

deriving via (Lift (ReaderT r) m) instance MonadEntityId m => MonadEntityId (ReaderT r m) 
deriving via (Lift (StateT s) m) instance MonadEntityId m => MonadEntityId (StateT s m)

instance (Monad (t m), MonadTrans t, MonadEntityId m) => MonadEntityId (Lift t m) where
  freshEntityId = Lift $ lift freshEntityId
  {-# INLINE freshEntityId #-}

runEntityId :: PrimMonad m => EntityIdM m a -> m a
runEntityId (EntityIdM m) = do
  arr <- newByteArray (sizeOf @Int undefined)
  runReaderT m $ EntityIdRef arr
{-# INLINE runEntityId #-}

instance MonadTrans EntityIdM where
  lift = EntityIdM . lift
  {-# INLINE lift #-}

instance PrimMonad m => MonadEntityId (EntityIdM m) where
  freshEntityId = EntityIdM $ do
    EntityIdRef (MutableByteArray ref) <- ask
    newId <- primitive $ \s ->
      case fetchAddIntArray# ref 0# 1# s of
        (# s', !i #) -> (# s', I# i #)
    pure $ EntityId newId
  {-# INLINE freshEntityId #-}

newtype EntityIdRef s = EntityIdRef (MutableByteArray s)