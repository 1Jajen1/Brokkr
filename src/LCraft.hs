{- |
Copyright: (c) 2022 Jannis
SPDX-License-Identifier: BSD-3-Clause
Maintainer: Jannis <overesch.jannis@gmail.com>

See README for more info
-}

module LCraft (
  startServer
) where

import Effectful
import Network (setupTCPServer)

startServer :: IO ()
startServer = do
  -- Load and init game state
  -- Start network stuff
  setupTCPServer
  -- Start game loop
  return ()

gameLoop :: Eff es ()
gameLoop = undefined


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

Task list:
  - Do network stuff
    - Setup framework for parsing packets and sending them
    - Initial custom builder implementation
      - Start simple here ...
    - Be able to respond to handshakes
  - Expand to have state on a connection and have some management state for the connection
    - Initially mutable, but later immutable as soon as the player joins
  - Implement command queue
    - Again start simple, tweak later
  - Start on the login sequence
    - Just make up data for now
    - Add first PlayerJoinCommand
      - Although I may be able to async that action
  - Implement NBT format
    - Simple stuff Map Text Tag etc
      Manual instances as generic kills compile time and isn't that performant (maybe later, this also takes a ton of time)
  - Implement chunk loading
    - Figure out a good abstract IO interface
    - Then hide that behind a region file interface
    - Think about caching, but don't implement that yet, just make it possible
    - Do the million subtasks involved in reading out chunks...
  - Start join sequence:
    - Keep alive handling
    - For now just send chunk packets and join the player right at 0,0
  - ...
-}

