module Command.Handler.Login (
  handle
) where

import Block.Position

import Command.Handler

import Control.Concurrent.STM
import Control.Monad.STM.Class

import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Network.Packet.Client.Play as C
import qualified Network.Packet.Client.Play.Login as C

import Client

import Registry.Biome
import Registry.Dimension

import Debug.Trace

handle :: Client -> Handler ()
handle p = do
    -- TODO all this is pretty temporary
  let loginData = C.LoginData
        (entityId p)
        (C.IsHardcore False)
        Creative
        C.Undefined
        (V.fromList ["minecraft:overworld", "minecraft:nether", "minecraft:the_end"])
        (C.RegistryCodec dimRegistry biomeRegistry chatRegistry)
        "minecraft:overworld" -- TODO proper type
        "minecraft:overworld"
        (C.HashedSeed 0)
        (C.MaxPlayers 5)
        (C.ViewDistance 8)
        (C.SimulationDistance 8)
        (C.ReducedDebugInfo False)
        (C.EnableRespawnScreen False)
        (C.IsDebug False)
        (C.IsFlat False)
        C.NoDeathLocation
      dimRegistry = C.DimensionTypeRegistry
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
      chatRegistry = C.ChatRegistry "minecraft:chat_type" mempty

  writePacket (connection p) 65536 (C.Login loginData)
  -- writePacket (connection p) 16 (C.SetDefaultSpawnPosition (BlockPos 0 100 0) 0)

  liftSTM $ writeTVar (joined p) True

  pure ()
