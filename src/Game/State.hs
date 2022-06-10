module Game.State (
  MonadGameState(..)
, GameState
, worlds, world
, players, player
, connections, connection
) where

import World (World, Dimension)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.UUID (UUID)
import Player (Player)
import qualified Network.Connection as Connection
import Optics
import Game.State.Internal
import Control.Monad.Trans.Reader
import Util.Lift
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

-- TODO Usage is very dangerous because of async errors, take/put is basically takeMVar/putMVar 
-- Ideally this is only used top level, but some async tasks need this
class Monad m => MonadGameState m where
  takeGameState :: m GameState
  putGameState  :: GameState -> m ()

deriving via (Lift (ReaderT r) m) instance MonadGameState m => MonadGameState (ReaderT r m)
deriving via (Lift (StateT s) m) instance MonadGameState m => MonadGameState (StateT s m)

instance (MonadGameState m, MonadTrans t, Monad (t m)) => MonadGameState (Lift t m) where
  takeGameState = Lift $ lift takeGameState
  {-# INLINE takeGameState #-}
  putGameState st = Lift . lift $ putGameState st
  {-# INLINE putGameState #-} 

--

worlds :: IxFold Dimension GameState World
worlds = reindexed toEnum $ ifolding _worlds
{-# INLINE worlds #-}

world :: Dimension -> Getter GameState World
world dim = to $ (flip V.unsafeIndex (fromEnum dim)) . _worlds
{-# INLINE world #-}

pLens :: Lens' GameState (HM.HashMap UUID Player)
pLens = lens _players $ \st nP -> st {_players = nP}
{-# INLINE pLens #-}

players :: IxTraversal' UUID GameState Player
players = pLens % itraversed
{-# INLINE players #-}

player :: UUID -> Lens' GameState (Maybe Player)
player uid = pLens % at uid
{-# INLINE player #-}

cLens :: Lens' GameState (HM.HashMap UUID Connection.Handle)
cLens = lens _connections $ \st nC -> st {_connections = nC}

connections :: IxTraversal' UUID GameState Connection.Handle
connections = cLens % itraversed
{-# INLINE connections #-}

connection :: UUID -> Lens' GameState (Maybe Connection.Handle)
connection uid = cLens % at uid
{-# INLINE connection #-}
