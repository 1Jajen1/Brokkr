{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Network.Effect.Network.IO (
  runNetwork
) where

import Network.Socket
import Effectful
import Network.Effect.Network.Effect
import Effectful.Dispatch.Dynamic (interpret)
import Network.Socket.ByteString
import qualified Data.ByteString as BS
import Data.Primitive.SmallArray (sizeofSmallArray)
import Data.Foldable

runNetwork :: IOE :> es => Socket -> Eff (Network : es) a -> Eff es a
runNetwork sock = interpret $ \_ -> \case
  ReceiveBytes -> do
    bs <- liftIO $ recv sock 512
    case BS.length bs of
      0 -> error "TODO closed"
      _ -> pure bs
  SendBytes bs -> do
    let _sz = sizeofSmallArray bs
        xs = toList bs -- TODO Can/should we optimize this?
    liftIO $ sendMany sock xs
