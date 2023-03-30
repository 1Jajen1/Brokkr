module Control.Monad.STM.Class (
  MonadSTM(..)
) where

import Control.Concurrent.STM

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS

class MonadSTM m where
  liftSTM :: STM a -> m a

instance MonadSTM STM where
  liftSTM = id

instance MonadSTM m => MonadSTM (ReaderT r m) where
  liftSTM = ReaderT . const . liftSTM

instance (Functor m, MonadSTM m) => MonadSTM (StateT s m) where
  liftSTM stm = StateT $ \s -> (,s) <$> liftSTM stm

instance (Functor m, Monoid w, MonadSTM m) => MonadSTM (WriterT w m) where
  liftSTM stm = writerT $ (,mempty) <$> liftSTM stm
