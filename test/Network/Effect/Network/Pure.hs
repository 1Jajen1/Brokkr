{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Network.Effect.Network.Pure (
  runNetwork
) where

import Network.Effect.Network.Effect
import Effectful
import Data.ByteString
import Effectful.Dispatch.Dynamic
import Effectful.Writer.Static.Local
import Data.Foldable
import Effectful.State.Static.Local

runNetwork :: [ByteString] -> Eff (Network : es) a -> Eff es (a, [ByteString])
runNetwork inputs = do
  reinterpret (runWriter . evalState inputs) $ \_ -> \case
    ReceiveBytes -> get >>= \case
      (x:xs) -> put xs >> pure x
      _ -> error "No more input!" -- TODO
    SendBytes arr -> tell $ toList arr
