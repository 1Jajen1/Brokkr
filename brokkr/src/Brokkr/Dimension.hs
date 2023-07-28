{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Brokkr.Dimension (
  Dimension
, entityId
, DimensionType(..)
, new
, DimensionName(..)
, RegionFilePath(..)
, withChunkLoading
) where

import Brokkr.Anvil.Chunk qualified as IO

import Brokkr.Chunk

import Brokkr.IO.ChunkCache (SomeChunkCache(..), newChunkCache, ChunkTicket, loadChunkRef)
import Brokkr.IO.Chunkloading (Chunkloading, loadChunk)

import Brokkr.Packet.ServerToClient.Play (ToHeightMapSize)

import Brokkr.Server.Monad

import Control.Concurrent.STM
import Control.Monad.IO.Class

import Data.Coerce
import Data.Proxy
import Data.Text (Text)
import Data.String (IsString)

import Foreign.Storable

import GHC.TypeLits

-- TODO Modify and add RegionFileFolderPath to this, so that we don't need too many lookups
-- also they cannot be logically seperate anyway..
newtype Dimension = Dimension EntityId
  deriving newtype (Eq, Storable)
  deriving Component via (ViaFlat Dimension)

entityId :: Dimension -> EntityId
entityId (Dimension eid) = eid
{-# INLINE entityId #-}

data DimensionType = Overworld | Nether | TheEnd

-- TODO Move to common DimensionSettings or something similar
newtype RegionFilePath = RegionFilePath String
  deriving newtype IsString

-- TODO Specialise
new :: forall (t :: DimensionType) dimHeight . (Has Universe t, KnownNat dimHeight, KnownNat (ToHeightMapSize dimHeight))
  => Chunkloading -> RegionFilePath -> EntityId -> Server Dimension
new chunkloading regionFolder dim = do
  addTag @t dim
  liftIO (newChunkCache @dimHeight (\(ChunkPosition x z) ->
      loadChunk (coerce regionFolder) (IO.ChunkPosition (fromIntegral x) (fromIntegral z)) chunkloading))
      >>= set dim . SomeChunkCache
  pure $ Dimension dim

withChunkLoading :: MonadHecs Universe m
  => Dimension
  -> (forall dimHeight . (KnownNat dimHeight, KnownNat (ToHeightMapSize dimHeight))
    => Proxy dimHeight -> (ChunkTicket -> ChunkPosition -> IO (TVar (Chunk dimHeight))) -> m a)
  -> m a
withChunkLoading dim act = do
  flip (get (coerce dim)) (error "TODO Errors") $ \(SomeChunkCache chunkCache) ->
    act Proxy (`loadChunkRef` chunkCache)

newtype DimensionName = DimensionName Text
  deriving stock Show
  deriving newtype Eq
  deriving Component via (ViaBox DimensionName)

