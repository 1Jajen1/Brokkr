{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Network.Handler.Play (
  playProtocol
, joinPlayer
) where
  
import Data.Text hiding (zip)
import Data.UUID
import Network.Monad
import Control.Monad.Reader.Class
import qualified Network.Connection as Connection
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT)
import Game.Monad (GameM)
import Player.Internal
import Player
import Util.Position
import Util.Rotation
import qualified Network.Packet.Server.Play as S
import qualified Command.Connection as Cmd
import qualified Data.Vector as V
import World
import Network.Protocol
import Util.Exception
import Game.State
import Optics
import Util.Disconnect
import Network.Connection

joinPlayer ::
  ( MonadIO m
  , MonadGameState m
  , MonadBracket m
  ) => Connection.Handle -> UUID -> Text -> m Connection.Handle
joinPlayer conn uid _name = do
  bracketOnError
    (modifyGameState $ connection uid .~ (Just conn))
    (\_ _ -> modifyGameState $ connection uid .~ Nothing)
    (const $ (liftIO $ pushCommand conn $ Cmd.JoinPlayer player) >> pure conn)
  where player = Player Creative (Position 0 200 0) (Rotation 0 0) OnGround Overworld uid

playProtocol ::
  ( MonadIO m
  , MonadBracket m
  , MonadReader Connection.Handle m
  , MonadNetwork m 
  ) => Protocol -> m ()
playProtocol prot = do
  conn <- ask

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
          -- TODO Handle. What am I even supposed to do here if the id does not match a prev teleport?
          S.TeleportConfirm _ -> pure ()
          -- TODO
          S.PlayerAbilities _ -> pure ()
          -- TODO
          S.EntityAction -> pure ()
          -- TODO
          S.KeepAlive _ -> pure ()
           -- TODO handle these?
          S.ClientSettings -> pure ()
          S.PluginMessage -> pure ()
          p -> error $ "Unhandled packet: " <> show p
        go
  bracketOnError (pure ()) (\_ e -> do
      case fromException @Connection.DisconnectException e of
        Just _ -> pure ()
        Nothing  -> liftIO $ pushCommand conn $ Cmd.Disconnect ClientDisconnect
    ) $ const go

{-# SPECIALIZE playProtocol :: Protocol -> ReaderT Connection.Handle (NetworkM (GameM IO)) () #-}
