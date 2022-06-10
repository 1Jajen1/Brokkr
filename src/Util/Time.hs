module Util.Time (
  MonadTime(..)
) where

import Control.Concurrent
import qualified Data.Time.Clock.POSIX as Time
import Control.Monad.Trans
import Util.Lift
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

class Monad m => MonadTime m where
  delay       :: Int -> m ()
  currentTime :: m Int

deriving via (Lift (ReaderT r) m) instance MonadTime m => MonadTime (ReaderT r m)
deriving via (Lift (StateT s) m) instance MonadTime m => MonadTime (StateT s m)

instance (Monad (t m), MonadTrans t, MonadTime m) => MonadTime (Lift t m) where
  delay = Lift . lift . delay
  {-# INLINE delay #-}
  currentTime = Lift $ lift currentTime
  {-# INLINE currentTime #-}

instance MonadTime IO where
  delay = threadDelay
  {-# INLINE delay #-}
  currentTime = round . (* 1000) <$> Time.getPOSIXTime
  {-# INLINE currentTime #-}
