module Event.Handler.PlayerJoined (
  playerJoined
) where

import qualified Network.Packet.Client.Play.JoinGame as C
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Network.Packet.Client.Play as C
import Network.Connection
import Game.State
import Optics
import Util.UUID
import Registry.Biome
import Registry.Dimension
import Player
import Entity.Id.Monad
import Control.Monad.IO.Class
import Block.Position
import Game.Monad (GameM)
import Control.Monad.State.Strict

playerJoined ::
  ( MonadIO m
  , MonadEntityId m
  , MonadState GameState m
  ) => Player -> m ()
playerJoined p = do
  entityId <- freshEntityId

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
      biomeRegistry = C.BiomeRegistry "minecraft:worldgen/biome" . V.fromList $ do
        (bid, (name, settings)) <- zip [0..] all_biome_settings
        return $ C.BiomeRegistryEntry ("minecraft:" <> T.pack name) bid settings
    
  -- TODO Update other players scoreboard etc.
  st <- get
  -- TODO? This should be safe, connections should not ne removed before this event is processed
  let Just conn = st ^. connection (p ^. uuid)
  liftIO . sendPackets conn $ V.fromListN 2 [
      (65536, C.JoinGame joinGameData)
    , (16, C.SpawnPosition (BlockPos 0 100 0) 0)
    ]
  
  put $ player uid .~ Just p $ st
  where uid = p ^. uuid
{-# SPECIALIZE playerJoined :: Player -> StateT GameState (GameM IO) () #-}