module Brokkr.STM.Class (
  MonadSTM(..)
) where

import Control.Concurrent.STM

class Monad m => MonadSTM m where
  liftSTM :: STM a -> m a

instance MonadSTM STM where
  liftSTM = id
