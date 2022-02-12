{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
--{-# OPTIONS_GHC -O -ddump-simpl-iterations -ddump-simpl-stats -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-module-prefixes -dsuppress-type-signatures -dsuppress-uniques #-}
module Util.Builder.Internal (
  Builder
, ensureBytes
, ensureBytes#
, boundedWrite
, fixedWrite
) where

import GHC.Exts

import Util.Builder.Prim (BuilderState, BoundedWrite(..), FixedWrite(..))

{-
Why this over existing Builders (bytestring, fast-builder, ...)?
  - Fun mainly, below are some other reasons but tbh those can probably be worked around with ByteString.Builder
  - Length prefixing. Minecraft is full of length prefixed data and ByteString.Builder can't do that easily afaik.
  - We know a lot of sizes, or at least can approximate them in most cases, so we can work with fixed size buffers a lot of the time
  - I kind of want to play around with custom allocators for buffers to manage memory myself and that is not possible with Builder afaik

This is heavily inspired by fast-builder/mason and the likes.
-}


{-
Goals:
- Abstract over how we receive buffers, Builder really only should concern itself with writing to and requesting new buffers

What does a Builder need:
  - A pointer to the start of the memory block that it can write to
  - A way to check how many bytes it is allowed to write
  - A way to ask for more bytes if it cannot write
-}

newtype Builder = Builder (AllocateBuffer -> BuilderState -> BuilderState)

instance Semigroup Builder where
  Builder f <> Builder g = Builder $ \aB bs -> g aB (f aB bs)
  {-# INLINE[1] (<>) #-}

-- This is basically Int -> IO (Ptr, Ptr) just unboxed
type AllocateBuffer = ((# Int#, State# RealWorld #) -> (# Addr#, Addr#, State# RealWorld #))

ensureBytes :: Int -> Builder
ensureBytes (I# sz) = Builder $ \aB (# c, e, wSz, s #) ->
  case ensureBytes# sz aB (# c, e, 0#, s #) of
    (# c', e', _, s' #) -> (# c', e', wSz, s' #)
{-# INLINE ensureBytes #-}

ensureBytes# :: Int# -> AllocateBuffer -> BuilderState -> BuilderState
ensureBytes# sz aB (# c, e, wSz, s #) =
  case sz <=# minusAddr# e c of
    1# -> (# c, e, wSz, s #)
    _ -> case aB (# sz, s #) of
      (# c', e', s' #) -> (# c', e', wSz, s' #)
{-# NOINLINE ensureBytes#  #-}

-- TODO Rules:
-- combine boundedWrite <> boundedWrite => boundedWrite
-- combine fixedWrite <> fixedWrite => fixedWrite
-- combine boundedWrite <> fixedWrite => boundedWrite
-- combine fixedWrite <> boundedWrite => boundedWrite
boundedWrite :: BoundedWrite a -> (a -> Builder)
boundedWrite (BoundedWrite sz f) = \a -> ensureBytes sz <> (Builder $ const (f a))
{-# INLINE[1] boundedWrite #-}

fixedWrite :: FixedWrite a -> (a -> Builder)
fixedWrite (FixedWrite sz f) = \a -> ensureBytes sz <> (Builder $ const (f a))
{-# INLINE[1] fixedWrite #-}
