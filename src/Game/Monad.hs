{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Game.Monad (
  MonadGame(..)
, GameM
, runGame
) where

import Entity.Id.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Concurrent
import qualified Network.Simple.TCP as Network
import qualified Network.Connection as Connection
import {-# SOURCE #-} Network (runProtocol)
import Network.Monad
import Util.Time
import Util.Log
import Control.Monad.Trans
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader
import Game.State
import Control.Concurrent.Async
import GHC.Conc (labelThread)
import Util.Exception

-- Make sure this is not available for either sync/async
-- TODO This should not exist
class
  ( MonadEntityId m
  , MonadTime m
  , MonadLog m
  , MonadGameState m
  ) => MonadGame m where
    setupNetwork :: m ()

-- TODO Move to app
newtype GameM m a = GameM { runGameM :: ReaderT (MVar GameState) (LogM 'Debug (EntityIdM m)) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadEntityId, MonadLog, MonadTime, PrimMonad)
  deriving anyclass MonadBracket

instance MonadTrans GameM where
  lift = GameM . lift . lift . lift
  {-# INLINE lift #-}

runGame :: (MonadIO m, PrimMonad m) => GameState -> GameM m a -> m a
runGame st act = do
  stRef <- liftIO $ newMVar st
  runEntityId . runLog . flip runReaderT stRef $ runGameM act
{-# INLINE runGame #-}

instance MonadIO m => MonadGameState (GameM m) where
  takeGameState = GameM ask >>= liftIO . takeMVar
  {-# INLINE takeGameState #-}
  putGameState !st = GameM ask >>= liftIO . flip putMVar st
  {-# INLINE putGameState #-}
  modifyGameState f = GameM ask >>= liftIO . flip modifyMVar_ (pure . f)
  {-# INLINE modifyGameState #-}

instance (MonadUnliftIO m, PrimMonad m, MonadTime m) => MonadGame (GameM m) where
  setupNetwork = do
    as <- withRunInIO $ \unlift -> async $ do
      myThreadId >>= flip labelThread ("Main network thread") 
      Network.serve (Network.Host "192.168.178.59") "25565" $ \(sock, _sockddr) -> do
        unlift (runNetwork sock $ runProtocol (Connection.new (Network.sendLazy sock))) >> pure ()
    liftIO $ link as
  {-# INLINE setupNetwork #-}
