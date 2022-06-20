{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Network.Handler.Play (
  playProtocol
) where
  
import Data.Text hiding (zip)
import Data.UUID
import Network.Monad
import Control.Monad.Reader.Class
import qualified Network.Connection as Connection
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT)
import Game.Monad (GameM)
import Game.Command (Command(JoinPlayer))
import Network.Connection.Internal
import Player.Internal
import Player
import Util.Position
import Util.Rotation
import qualified Network.Packet.Server.Play as S
import qualified Game.Command as Cmd
import qualified Data.Vector as V
import World
import Network.Protocol

playProtocol ::
  ( MonadIO m
  , MonadReader Connection.Handle m
  , MonadNetwork m
  ) => Protocol -> UUID -> Text -> m ()
playProtocol prot uid _text = do
  let player = Player Creative (Position 0 200 0) (Rotation 0 0) OnGround Overworld uid

  conn <- ask
  liftIO . pushCommand conn $ JoinPlayer player

  let go = do
        -- TODO Instead of parsing the packet we could operate on the binary form and only copy the bytes we actually need
        -- Saves at least one allocation, a lot more if the client sends nbt
        readPacket @S.PlayPacket prot >>= \case
          S.PlayerPosition pos onGround -> liftIO . pushCommands conn $ V.fromListN 2 [
              Cmd.MovePlayer pos
            , Cmd.UpdateMovingPlayer onGround
            ]
          S.PlayerRotation rot onGround -> liftIO . pushCommands conn $ V.fromListN 2 [
              Cmd.RotatePlayer rot
            , Cmd.UpdateMovingPlayer onGround
            ]
          S.PlayerPositionAndRotation pos rot onGround -> liftIO . pushCommands conn $ V.fromListN 3 [
              Cmd.MovePlayer pos
            , Cmd.RotatePlayer rot
            , Cmd.UpdateMovingPlayer onGround
            ]
          S.PlayerMovement onGround -> liftIO $ pushCommand conn $ Cmd.UpdateMovingPlayer onGround
          _ -> pure () -- Error here?
        go

  go

{-# SPECIALIZE playProtocol :: Protocol -> UUID -> Text -> ReaderT Connection.Handle (NetworkM (GameM IO)) () #-}
