module LCraft (

) where

{-

Entities will be made up of components, so that is what will be shared

Player {
  Position
  Velocity
  Health
  Inventory
  ...
}

These things have no hierarchy, just add them together and add lenses into them to modify.

Mutable bits are wrapped in TVar's, ideally unlifted to never build up lazy thunks

TVar boundaries:
  - Position, Velocity would be in one TVar and not split for x,y,z since they will almost always be manipulated together
    - also it'd be a waste of space
  - Inventory would not be in a TVar because it itself won't be mutated, only its contents, which will be in TVar's
    - Well that sort of depends on how I encode it...
  - Blockstates in a chunk are in one TVar, wasteful to hide them behind even more pointers

-}
