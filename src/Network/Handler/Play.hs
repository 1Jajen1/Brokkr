{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Network.Handler.Play (
  finalizeLogin
, playProtocol
) where
import Effectful
import Data.Text hiding (zip)
import Data.UUID
import Network.Effect.Network
import Effect.EntityId
import qualified Network.Packet.Server.Play as S
import qualified Network.Packet.Client.Play as C
import qualified  Network.Packet.Client.Play.JoinGame as C
import Network.Packet.Client.Play.ChunkData (mkChunkData)
import Network.Effect.Packet
import Player.Internal
import qualified Data.Vector as V
import qualified Data.Text as T
import Registry.Dimension
import Registry.Biome
import Block.Position
import Effect.World
import World.Internal -- TODO Revise use of internal everywhere
import Chunk.Position
import Util.Position
import Control.Monad.Fix
import Util.Binary
import Data.ByteString (ByteString)
import Effectful.State.Static.Local (evalState)
import Util.Rotation
import Control.Monad (forM_)
import Debug.Trace
import Effect.ChunkManager (loadChunk)
import Effect.Async

finalizeLogin ::
  ( Async :>> es
  , Network :> es
  ) => UUID -> Text -> Eff es ()
finalizeLogin _uid _uname = do
  entityId <- traceShowId <$> freshEntityId

  {- TODO:
    Load player profile from storage or create new one
    Load world the player is in
    
    Send the JoinGame and SpawnPosition packet
    
    Load the chunks around the player

    Send all the chunks as they are loaded

    Queue a JoinPlayer command on this connection

  -}

  -- TODO all this is pretty temporary
  let joinGameData = C.JoinGameData
        entityId
        (C.IsHardcore False)
        Creative
        C.Undefined
        (V.fromList ["minecraft:overworld", "minecraft:nether", "minecraft:the_end"])
        (C.DimensionCodec dimRegistry biomeRegistry)
        overworld
        "minecraft:overworld"
        (C.HashedSeed 0)
        (C.MaxPlayers 5)
        (C.ViewDistance 8)
        (C.ReducedDebugInfo False)
        (C.EnableRespawnScreen False)
        (C.IsDebug False)
        (C.IsFlat False)
      dimRegistry = C.DimensionType
        "minecraft:dimension_type"
        (V.fromList
          [
            C.DimensionRegistryEntry "minecraft:overworld" 0 overworld
          , C.DimensionRegistryEntry "minecraft:nether" 1 nether
          , C.DimensionRegistryEntry "minecraft:the_end" 2 end
          ]
        )
      --biomeRegistry = C.BiomeRegistry "minecraft:worldgen/biome" mempty
      biomeRegistry = C.BiomeRegistry "minecraft:worldgen/biome" . V.fromList $ do
        (bid, (name, settings)) <- zip [0..] all_biome_settings
        return $ C.BiomeRegistryEntry ("minecraft:" <> T.pack name) bid settings

  -- TODO Should probably cache this
  sendPacket 65536 $ C.JoinGame joinGameData
  sendPacket 16 $ C.SpawnPosition (BlockPos 0 100 0) 0

  -- send chunks
  sendPacket 10 $ C.UpdateViewPosition 0 0
  withWorld Overworld $ \world -> forM_ [(x,y) | x <- [-8..8], y <- [-8..8]] $ \(x,y) -> do
    !chunk <- loadChunk (world.chunkManager) $ ChunkPos x y
    let (!cSize, !cData) = mkChunkData chunk
    sendPacket cSize $ C.ChunkDataAndUpdateLight cData

    pure ()
  
  -- TODO Handle TeleportId
  -- send initial position
  sendPacket 40 $ C.PlayerPositionAndLook (Position 0 200 0) (Rotation 0 0) (C.TeleportId 0) C.NoDismount

playProtocol ::
  ( Network :> es
  , Async :>> es
  ) => UUID -> Text -> Eff es ()
playProtocol uuid text = do
  -- TODO Some bracket stuff is needed to make sure we clean up properly if the connection closes
  -- Up until now we have only connection local state, but when finalizeLogin finishes we will have added
  -- JoinPlayer as a command and thus have committed to modifying state
  finalizeLogin uuid text
  evalState (mempty @ByteString) $ fix $ \loop -> do
    _ <- readPacket (get @S.PlayPacket)
    loop

-- TOOD 
-- Figure out how to do KeepAlives, for now just fork a thread when running playProtocol
-- Write updatePosition :: Player -> Maybe Position -> Effs to handle UpdateViewPosition and ChunkUpdates
