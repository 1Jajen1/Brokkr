{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module LCraft (
  startServer
) where

import Game.Monad
import Util.Time
import Util.Log
import Control.Monad
import qualified Data.Text as T
import Control.Monad.State.Strict
import Game.State
import Command.Connection
import Sync.Monad
import Sync.Handler.JoinPlayer (joinPlayer)
import Sync.Handler.PlayerMovement (movePlayer, rotatePlayer, updateMovingPlayer)
import Sync.Handler.DisconnectPlayer (disconnectPlayer)
import Optics
import Optics.Operators.Unsafe
import Util.UUID
import Control.Concurrent.Async
import Control.Monad.IO.Unlift
import GHC.Conc (myThreadId, labelThread)
import Network.Connection
import qualified World.Internal as World
import qualified Util.Queue as Queue
import qualified Command.Handler.World
import World.Dimension
import qualified Event.Handler

-- All of this should not be in the lib
startServer :: (MonadUnliftIO m, MonadGame m) => m ()
startServer = do
  -- Start network stuff
  setupNetwork
  -- Start game loop
  as <- withRunInIO $ \runInIO -> do 
    async $ do
      myThreadId >>= flip labelThread ("Main gameloop")
      runInIO $ gameLoop $ do
        !initSt <- takeGameState
        !(_, !newSt) <- flip runStateT initSt $ do

          forOf_ worlds initSt $ \w -> do
            !commands <- liftIO $ Queue.flush $ World._commandQueue w
            let dim = w ^. dimension
            forM_ commands $ \cmd -> do
              st <- get
              let w' = st ^. world dim
                  evs = runSync $ Command.Handler.World.handle cmd w'
              
              forM_ evs Event.Handler.apply 

          forOf_ connections initSt $ \conn -> do
            !commands <- liftIO $ flushCommands conn
            forM_ commands $ \cmd -> do
              st <- get

              -- Here st is readonly
              -- TODO Handle player disconnecting
              -- TODO Move this somewhere else and start moving all of this out to app
              let p = st ^?! player (conn ^. uuid) % _Just
                  evs = runSync $ case cmd of
                    (JoinPlayer p') -> joinPlayer p' st
                    (MovePlayer pos) -> movePlayer p pos st
                    (RotatePlayer rot) -> rotatePlayer p rot st
                    (UpdateMovingPlayer onGround) -> updateMovingPlayer p onGround st
                    Disconnect r -> disconnectPlayer p r

              -- apply events and send packets
              forM_ evs Event.Handler.apply

        liftIO $ print newSt
        putGameState newSt

  liftIO $ link as

  fix $ \loop -> do
    inp <- liftIO getLine
    case inp of
      "stop" -> liftIO $ cancel as
      _ -> loop
{-# SPECIALIZE startServer :: GameM IO () #-}

gameLoop :: MonadGame m => m () -> m ()
gameLoop act = go
  where
    go = do
      !start <- currentTime

      act

      !end <- currentTime

      let !diff = max 0 $ end - start
      when (diff > 10) . logWarn . T.pack $ "Tick took: " <> show diff

      delay $ 50 * 1000 - diff

      go
{-# INLINE gameLoop #-}

{-

Ideas:
- Entity:
data Entity ([l] :: [EntityCapabilities]) where
  Creeper :: Entity [Living, Mob]
  Chicken :: Entity [Living, Ageable, Passive, Breedable]

instance Contains Breedable ls => Breedable (Entity ls) where
  breed :: ...

-- Some kind of type error when we are checking for a known instance

-}

{-

Network:
Parsing: Use flatparse for packets
NBT: For now just parse into Map (Not HashMap, it's not necessary and we don't want attack surfaces)

Encoding:
Custom Builder:
  - Why? Because of size prefixing, this is hard/annoying to do with existing builder interfaces
  - Have fixed size allocators and use some runtime information as well
  - Prefixing is simply leaving space at the beginning
  - Use custom allocator, essentially just a bump alloc with some strategy for growing


IO:
  - Do IO over io_uring and ignore haskells io manager:
    - this includes network IO
    - Why? Why not!
  - Start with haskells IO manager tho


Stuff:
  Abstract IO so that we can switch to a different IO manager at any time
  Use a custom builder day 1, but astract allocation if possible
  Use gc everywhere and don't do manual memory management day 1, but keep in mind that that may happen at some point
  Figure out some way to reliably start ticks on time:
    - Start with just a haskell thread, but keep other options open
    - Maybe just a external thread that is not managed by haskells scheduler and calls into haskell?

NBT parser:
  For starters just parse into a Map/HashMap
  Later:
  Write NBT Values to a Tape as we parse them and later parse that tape
    - The first step can be done using simd instructions and could be really really fast
    - The second step can be done using builder structs (just a builder on a ptr and later read from the storable)
  Custom allocator here since values are increadibly short lived and can be freed as soon as the builders are done, no need to trouble the gc here.

Main game loop: Command -> State -> [Event]
After the game loop [Event] -> Network packets and State -> [Event] -> State. Use dataToTag for filters and specialized datastructures.
Queues or just buffers for command/event storage. Queues are overkill I think because I read out all items in frequent intervals rather than
trying to empty it as fast as possible. So backpressure concepts etc are all useless and not wanted here. So just go for smth like an array list...
Concurrency during a tick is going to be "fun". Need to avoid races when reading state, but stm is a little slow for hot loops. State technically changes
during a tick, just not unobservably through mutability. Events are state changes and thus to prevent races state access has to consider Events that change said state.
There are probably some smart ways to avoid needing locks: E.g. group commands by location to avoid having to lock chunks

Task list:
||  - Do network stuff
||    - Setup framework for parsing packets and sending them
||    - Initial custom builder implementation
||      - Start simple here ...
||    - Be able to respond to handshakes
  - Expand to have state on a connection and have some management state for the connection
    - Initially mutable, but later immutable as soon as the player joins
||  - Implement command queue
||    - Again start simple, tweak later
||  - Start on the login sequence
||    - Just make up data for now
||    - Add first PlayerJoinCommand
||      - Although I may be able to async that action. Most of it can be done async, but the actual modification of server state beyond the connection manager needs
          to be done sync
||  - Implement NBT format
||    - Simple stuff Map Text Tag etc
||      Manual instances as generic kills compile time and isn't that performant (maybe later, this also takes a ton of time)
||  - Implement chunk loading
||    - Figure out a good abstract IO interface
||    - Then hide that behind a region file interface
||    - Think about caching, but don't implement that yet, just make it possible
||    - Do the million subtasks involved in reading out chunks...
||  - Start join sequence:
||    - Keep alive handling
||    - For now just send chunk packets and join the player right at 0,0
  - Implement player movement
    - Basic position updates and chunkloading
    - implement creative mode flying
-}

