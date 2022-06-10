{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Util.Log (
  MonadLog(..)
, logDebug
, logInfo
, logWarn
, logError
, LogM
, runLog
, LogLevel(..)
) where

import Data.Text
import Prelude hiding (log)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Primitive
import Entity.Id.Monad
import Util.Time
import Control.Monad.IO.Unlift
import Control.Monad.Trans.State.Strict
import Util.Lift

-- TODO Extend this to add much more sophisticated logging
-- I'd love to have optional logging contexts such as time/tick
-- other arbitrary context
class Monad m => MonadLog m where
  log :: LogLevel -> Text -> m ()
  context :: (Text -> Text) -> m a -> m a

logDebug :: MonadLog m => Text -> m ()
logDebug = log Debug
{-# INLINE logDebug #-}

logInfo :: MonadLog m => Text -> m ()
logInfo = log Info
{-# INLINE logInfo #-}

logWarn :: MonadLog m => Text -> m ()
logWarn = log Warn
{-# INLINE logWarn #-}

logError :: MonadLog m => Text -> m ()
logError = log Error
{-# INLINE logError #-}

data LogLevel = Debug | Info | Warn | Error
  deriving stock (Show, Eq, Ord)

newtype LogM (lvl :: LogLevel) m a = LogM { runLogM :: ReaderT (Text -> Text) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadEntityId, PrimMonad)
  deriving MonadTime via (Lift (LogM lvl) m)

instance MonadTrans (LogM lvl) where
  lift = LogM . lift
  {-# INLINE lift #-}

instance MonadLog m => MonadLog (ReaderT r m) where
  log lvl text = lift $ log lvl text
  {-# INLINE log #-}
  context ctx act = ask >>= (lift . context ctx . runReaderT act)
  {-# INLINE context #-}

instance MonadLog m => MonadLog (StateT s m) where
  log lvl text = lift $ log lvl text
  {-# INLINE log #-}
  context ctx act = do
    s <- get
    (a, s') <- lift $ context ctx (runStateT act s)
    put s' >> pure a
  {-# INLINE context #-}

runLog :: LogM lvl m a -> m a
runLog = flip runReaderT id . runLogM
{-# INLINE runLog #-}

class KnownLogLevel lvl where
  logLevel :: LogLevel

instance KnownLogLevel 'Info where
  logLevel = Info
instance KnownLogLevel 'Debug where
  logLevel = Debug
instance KnownLogLevel 'Warn where
  logLevel = Warn
instance KnownLogLevel 'Error where
  logLevel = Error

instance (KnownLogLevel lvl, MonadIO m) => MonadLog (LogM lvl m) where
  log lvl txt = LogM $ do
    ctx <- ask
    when (lvl >= minLvl) $ lift $ liftIO . putStrLn . T.unpack $ ctx txt
    where minLvl = logLevel @lvl
  {-# INLINE log #-}
  context newCtx = LogM . local (\ctx -> newCtx . ctx) . runLogM
  {-# INLINE context #-}
