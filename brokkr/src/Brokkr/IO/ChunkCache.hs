{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
module Brokkr.IO.ChunkCache (
  SomeChunkCache(..)
, ChunkCache
, newChunkCache
, ChunkTicket(..)
, loadChunkRef
) where

import Brokkr.Anvil.Chunk qualified as IO

import Brokkr.Chunk

import Brokkr.Packet.ServerToClient.Play (ToHeightMapSize)

import Control.Concurrent
import Control.Concurrent.STM

import Data.Bitfield.Internal (Bitfield(Bitfield))

import Data.Bits
import Data.Coerce

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS

import GHC.Exts ( Int#, Int(I#), MVar#, RealWorld, State# )
import GHC.IO ( IO(IO) )
import GHC.MVar ( MVar(MVar) )

import GHC.TypeLits

import Hecs (Component, ViaBox, EntityId)
import Hecs.Entity.Internal (EntityId(EntityId))

data SomeChunkCache = forall dimHeight . (KnownNat dimHeight, KnownNat (ToHeightMapSize dimHeight)) => SomeChunkCache {-# UNPACK #-} !(ChunkCache dimHeight)
  deriving Component via (ViaBox SomeChunkCache)

-- | A chunk cache
data ChunkCache dimHeight = ChunkCache {
  chunkCacheLoadChunk# :: Int# -> Int# -> State# RealWorld -> (# State# RealWorld, MVar# RealWorld IO.Chunk #)
  -- why STM? MVar's here seem impossible to do safely, we don't want a full lock on all chunks, but a
  -- lock per chunk will invite deadlocks or races
, chunkCacheLoaded  :: TVar (IntMap (TVar (CachedChunk dimHeight))) -- Bit wasteful. TODO Write a STM version of IntMap
, chunkCachePending :: TVar (IntMap (PendingChunk dimHeight)) -- TODO Put PendingChunk into a TVar
}
  deriving Component via (ViaBox (ChunkCache dimHeight))

newChunkCache :: (ChunkPosition -> IO (MVar IO.Chunk)) -> IO (ChunkCache dimHeight)
{-# INLINE newChunkCache #-}
newChunkCache loadFromDisk = do
  chunkCacheLoaded  <- newTVarIO mempty
  chunkCachePending <- newTVarIO mempty
  pure $ ChunkCache {..}
  where
    chunkCacheLoadChunk# x z s =
      case loadFromDisk (ChunkPosition (I# x) (I# z)) of
        IO f -> case f s of
          (# s', MVar mv #) -> (# s', mv #)

-- Manual worker wrapper for chunkloading, not terribly needed but why not
chunkCacheLoadChunk :: ChunkCache dimHeight -> ChunkPosition -> IO (MVar IO.Chunk)
{-# INLINE chunkCacheLoadChunk #-}
chunkCacheLoadChunk ChunkCache{chunkCacheLoadChunk#} (ChunkPosition (I# x) (I# z)) = IO $ \s ->
  case chunkCacheLoadChunk# x z s of (# s', mv #) -> (# s', MVar mv #)

data CachedChunk dimHeight = CachedChunk {
  chunkTickets :: !IntSet
, cachedChunk  :: !(TVar (Chunk dimHeight))
}

data PendingChunk dimHeight = PendingChunk {
  pendingTickets :: !IntSet
, pendingChunk   :: !(TMVar (TVar (CachedChunk dimHeight)))
}

-- Player loaded areas use the player eid
-- Temporary loads have to allocate an entity id
-- Portals? Idk
newtype ChunkTicket = ChunkTicket EntityId

data LoadRes dimHeight = Loaded !(TVar (Chunk dimHeight)) | Pending !(TMVar (TVar (CachedChunk dimHeight))) | NotLoaded

-- Get the reference to a chunk. Blocks if the chunk is not in the cache
loadChunkRef :: forall dimHeight . (KnownNat dimHeight, KnownNat (ToHeightMapSize dimHeight))
  => ChunkTicket -> ChunkCache dimHeight -> ChunkPosition -> IO (TVar (Chunk dimHeight))
loadChunkRef ticket cache pos@(ChunkPosition x z) = do
  -- First transaction. Here we add our ticket and short circuit if the chunk is already loaded
  loadRes <- atomically $ do
    -- is already loaded?
    loaded <- readTVar $ chunkCacheLoaded cache
    case IM.lookup posKey loaded of
      -- already loaded, just return the reference
      Just ref -> do
        -- add the ticket
        cc <- readTVar ref
        writeTVar ref $! cc { chunkTickets = IS.insert (coerce ticket) $ chunkTickets cc }
        pure $ Loaded (cachedChunk cc)
      -- Not loaded, maybe someone already requested it?
      Nothing -> do
        pending <- readTVar $ chunkCachePending cache
        case IM.lookup posKey pending of
          -- A request is already in flight, just loop on that
          Just pendingReq -> do
            -- add our ticket to the request. We cannot do this after this transaction because we need to avoid races
            writeTVar (chunkCachePending cache) $! IM.insert posKey (pendingReq { pendingTickets = IS.insert (coerce ticket) $ pendingTickets pendingReq }) pending
            pure . Pending $ pendingChunk pendingReq
          -- No request yet, create one
          Nothing -> do
            resV <- newEmptyTMVar
            writeTVar (chunkCachePending cache) $! IM.insert posKey (PendingChunk (IS.singleton (coerce ticket)) resV) pending
            -- The actual request is IO and done outside of the transaction, we just
            -- inform other threads here that we will enqueue the load
            pure NotLoaded
  case loadRes of
    -- Already done
    Loaded tv -> pure tv
    -- block until the chunkload is done
    Pending tmv -> atomically $ readTMVar tmv >>= fmap cachedChunk . readTVar
    -- enqueue the chunkload, block until it is done and add to the correct places
    NotLoaded -> do
      -- What do we do if this errors?
      chunkCacheLoadChunk cache pos >>= takeMVar >>= \ioChunk ->
        atomically $ do
          pendingMap <- readTVar (chunkCachePending cache)
          case IM.lookup posKey pendingMap of
            Nothing -> error "Someone removed our pending request?!"
            Just (PendingChunk tickets tmv) -> do
              let chunk = fromIOChunk ioChunk
              !chunkRef <- newTVar chunk
              !ref <- newTVar $! CachedChunk tickets chunkRef
              writeTMVar tmv ref
              writeTVar   (chunkCachePending cache) $! IM.delete posKey pendingMap
              modifyTVar' (chunkCacheLoaded  cache) $  IM.insert posKey ref
              pure chunkRef
  where
    posKey = ((0xFFFFFFFF .&. x) `unsafeShiftL` 32) .|. (0xFFFFFFFF .&. z)
