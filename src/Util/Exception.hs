{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
module Util.Exception (
  MonadBracket(..)
, bracketOnError
, SomeException(..)
, Exception.Exception(..)
) where

-- MOVE
import Control.Monad.IO.Unlift
import qualified Control.Exception as Exception
import Control.Monad.Trans.Reader
import Control.Exception (SomeException)
import Control.Monad (void)

-- Wrapper around bracket style methods
-- Useful for types that should not implement MonadUnliftIO (StateT and related) but still need this
-- functionality. Currently this is only really for NetworkM as it sits atop StateT and requires
-- bracket with the additional benefit that in those cases it never uses the state in acquire or release
class Monad m => MonadBracket m where
  bracket :: m a -> (a -> Maybe SomeException -> m ()) -> (a -> m b) -> m b
  default bracket :: MonadUnliftIO m => m a -> (a -> Maybe SomeException -> m ()) -> (a -> m b) -> m b
  bracket acq clean act = withRunInIO $ \unlift ->
    Exception.mask $ \restore -> do
      a <- unlift acq
      res <- Exception.catch (restore (unlift $ act a)) $ \e -> do
        void . unlift . clean a $ Just e
        Exception.throwIO e
      void . unlift $ clean a Nothing
      pure res
  {-# INLINE bracket #-}

bracketOnError :: MonadBracket m => m a -> (a -> SomeException -> m ()) -> (a -> m b) -> m b
bracketOnError acq clean act = bracket acq (\a -> \case
  Just e -> clean a e
  Nothing -> pure ())
  act 
{-# INLINE bracketOnError #-}

instance MonadBracket m => MonadBracket (ReaderT r m) where
  bracket acq clean act = ReaderT $ \r -> bracket (runReaderT acq r) (\a exc -> flip runReaderT r $ clean a exc) (flip runReaderT r . act)
  {-# INLINE bracket #-}