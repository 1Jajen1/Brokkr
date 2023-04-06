module Server.Config (
  Config(..)
, defaultConfig
) where

import Network.Simple.TCP qualified as Network

data Config = Config {
  configHostPreference :: !Network.HostPreference
, configServiceName    :: !Network.ServiceName
, configChunkloadingThreads  :: {-# UNPACK #-} !Int
, configServerRenderDistance :: {-# UNPACK #-} !Int
, configRootPath :: !String
}

defaultConfig :: Config
defaultConfig = Config {
  configHostPreference = Network.Host "127.0.0.1"
, configServiceName    = "25565"
, configChunkloadingThreads  = 16
, configServerRenderDistance = 48
, configRootPath = "./server"
}
