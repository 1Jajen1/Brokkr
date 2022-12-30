{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Client.Internal (
  Client(..)
, new
, update
, getPosition, setPosition
, getRotation, setRotation
) where

import Chunk.Position
import Client.GameMode

import Control.Concurrent.STM
import Control.Monad

import Data.Bits

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Data.UUID

import Dimension (Dimension)
import qualified Dimension

import Entity.EntityId (EntityId)

import Network.Connection (Connection)
import qualified Network.Connection as Conn
import qualified Network.Packet.Client.Play as C

import Optics

import Util.Position
import Util.Rotation
import Util.UUID

data Client = Client {
  connection   :: Connection
, gameMode     :: TVar GameMode
, joined       :: TVar Bool
, loadedChunks :: TVar (HM.HashMap ChunkPosition LoadedChunk)
, dimension    :: TVar Dimension
, entityId     :: EntityId
, _uuid        :: UUID
, mut          :: TVar ClientMut -- Stuff that frequently changes and also should be layed out close and unboxed in memory
                                 -- This comes at the cost of more memory copied and possibly more retries in transactions
}

data ChunkTicket
data Chunk
data LoadedChunk = LoadedChunk ChunkTicket Chunk

data ClientMut = ClientMut {
  position    :: Position
, oldPosition :: Position
, rotation    :: Rotation
, changes     :: ClientChanges
}

newtype ClientChanges = ClientChanges Word

instance Semigroup ClientChanges where
  ClientChanges a <> ClientChanges b = ClientChanges $ a .|. b

instance Monoid ClientChanges where
  mempty = ClientChanges 0

posChanged :: Lens' ClientChanges Bool
posChanged = lens
  (\(ClientChanges w) -> testBit w 1)
  (\(ClientChanges w) -> \case
    True -> ClientChanges $ setBit w 1
    False -> ClientChanges $ clearBit w 1
    )

-- TODO Actually load the client
new :: Connection -> Dimension -> EntityId -> UUID -> IO Client
new connection dim entityId _uuid = do
  gameMode     <- newTVarIO Creative
  joined       <- newTVarIO False
  loadedChunks <- newTVarIO mempty
  dimension    <- newTVarIO dim
  mut          <- newTVarIO $ ClientMut {
    position    = Position 0 100 0
  , oldPosition = Position 0 100 0
  , rotation    = Rotation 0 0
  , changes     = mempty
  }
  pure Client{..}

instance HasUUID Client where
  uuid Client{..} = _uuid

getPosition :: Client -> STM Position
getPosition Client{mut} = position <$> readTVar mut

setPosition :: Client -> Position -> STM ()
setPosition Client{mut} pos = modifyTVar' mut $ \ClientMut{..} -> ClientMut{position = pos, changes = set posChanged True changes, ..}

getRotation :: Client -> STM Rotation
getRotation Client{mut} = rotation <$> readTVar mut

setRotation :: Client -> Rotation -> STM ()
setRotation Client{mut} rot = modifyTVar' mut $ \ClientMut{..} -> ClientMut{rotation = rot, ..}

update :: Client -> IO ()
update Client{..} = do
  ClientMut{..} <- readTVarIO mut
  loadedChunks' <- readTVarIO loadedChunks

  -- position and chunk changes
  when (changes ^. posChanged) $ do
    let center@(ChunkPos centerX centerZ) = position ^. chunkPosition
        oldCenter = oldPosition ^. chunkPosition
        viewDistance = 8 -- TODO
    when (center /= oldCenter) $ do
      Conn.sendPacket connection . Conn.SendPacket 16 $ C.SetCenterChunk centerX centerZ
      let inView = chunksInView center (viewDistance + 2)
          loaded = HM.keysSet loadedChunks'
          toLoad = HS.difference inView loaded
          toUnload = HS.difference loaded inView

      -- iterate over all unload packets, send unload packets and invalidate all player tickets
        -- remove them from the loaded chunks

      -- create a single IORef and just atomicModify the inserts done by loading chunks

      -- iterate over all toLoad chunks and await loading them
        -- add them to the loaded chunks
      forM_ toLoad $ \cp -> do
        pure ()
      pure ()
    
    -- iterate over all loaded chunks and pull their packets

    pure ()
  
  -- iterate all entities this player can see and pull their changes

  atomically $ do
    writeTVar mut $ ClientMut {
      position    = position
    , oldPosition = position
    , rotation    = rotation
    , changes     = mempty
    }

-- utils

-- TODO Make this a static hashSet and simply iterate it and add the center later 
chunksInView :: ChunkPosition -> Int -> HS.HashSet ChunkPosition
chunksInView (ChunkPos cX cZ) rad = HS.fromList [ChunkPos (cX + x) (cZ + z) | x <- [-rad..0] <> [0..rad],z <- [-rad..0] <> [0..rad]]

-- TODO List
-- 1. Change Player to client - DONE (mostly)
-- 2. Add entities
-- 3. Handle player updates (oldPosition, etc.)
-- 4. Chunkloading (Get chunks right this time)
