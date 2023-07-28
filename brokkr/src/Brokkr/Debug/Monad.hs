{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Debug.Monad (
  DebugLevel(..)
, TraceT(..)
, runTraceTStdOut
, MonadTrace
, context
, debug
) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Trans
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader

import Data.Proxy
import Hecs.Monad.Class

data DebugLevel = Verbose | Debug | Info | Error
  deriving stock (Eq, Ord)

class KnownDebugLevel (lvl :: DebugLevel) where
  lvlVal :: Proxy lvl -> DebugLevel

instance KnownDebugLevel Verbose where
  lvlVal _ = Verbose
instance KnownDebugLevel Debug where
  lvlVal _ = Debug
instance KnownDebugLevel Info where
  lvlVal _ = Info
instance KnownDebugLevel Error where
  lvlVal _ = Error


newtype TraceT (level :: DebugLevel) m a = TraceT (ReaderT TraceSettings m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b, MonadTrans, MonadHecs u)

runTraceTStdOut :: TraceT lvl m a -> m a
runTraceTStdOut (TraceT act) = runReaderT act $ TraceSettings putStrLn ""

data TraceSettings = TraceSettings {
  outF :: String -> IO ()
, ctx :: String
}

class MonadTrace m where
  context' :: forall (lvl :: DebugLevel) a . KnownDebugLevel lvl => Proxy lvl -> String -> m a -> m a
  output :: forall (lvl :: DebugLevel) . KnownDebugLevel lvl => Proxy lvl -> String -> m ()

instance (KnownDebugLevel level, MonadIO m) => MonadTrace (TraceT level m) where
  context' p ctx' (TraceT act)
    | lvlVal (Proxy @level) <= lvlVal p = TraceT $ do
      TraceSettings{..} <- ask
      -- TODO When I eventually collect traces differently I may want to reuse this
      -- liftIO $ outF $ "├ " <> ctx

      -- start <- liftIO $ Chronos.getTime <$> Chronos.now

      let newCtx = case ctx of [] -> ctx'; xs -> xs <> "." <> ctx'
      res <- local (const $ TraceSettings outF newCtx) act

      -- end <- liftIO $ Chronos.getTime <$> Chronos.now

      -- let diff = min 0 $ end - start

      -- liftIO $ outF $ "│ └ finished in " <> show diff <> "ns"

      pure res
    | otherwise = TraceT act
  output p str
    | lvlVal (Proxy @level) <= lvlVal p =  TraceT $ do
      TraceSettings{..} <- ask
      liftIO $ outF $ ctx <> ": " <> str
    | otherwise = pure () 

context :: forall (lvl :: DebugLevel) m a . (KnownDebugLevel lvl, MonadTrace m) => String -> m a -> m a
context = context' (Proxy @lvl)

debug :: forall (lvl :: DebugLevel) m . (KnownDebugLevel lvl, MonadTrace m) => String -> m ()
debug = output (Proxy @lvl)
