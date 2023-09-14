{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- {-# OPTIONS_GHC -ddump-cmm #-}
module Hecs.Filter.Internal (
  Filter
, runFilter
, extractMain
, componentWithId
, filterAnd, And
, filterNot, Not
, TypedArchetype(..)
, foldTypedArchetype
, FoldBitSets
) where

import Prelude hiding (and)

import Control.Monad.ST.Strict (ST, runST)
import Control.Monad.Primitive

import Data.Bitfield
import Data.Bits ((.&.), complement, xor, countTrailingZeros)
import Data.Coerce
import Data.Kind
import Data.Primitive
import Data.Type.Bool qualified as Bool
import Data.Void
import Data.Word

import Foreign.Storable (Storable)

import GHC.Exts

import Hecs.Archetype.Internal (Archetype)
import Hecs.Archetype.Internal qualified as Archetype
import Hecs.Component.Internal
import Hecs.Component.Relation
import Hecs.Entity.Internal
import Hecs.World.Has

import Debug.Trace

-- Everything in this module has INLINE pragmas so that the any filter compiles to a constant main id and a constant matcher

-- TODO Make this more efficient if it fails to inline

newtype PosBuilder = PosBuilder (Int, forall s . MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #))

mkPosBuilder :: Int -> (forall s . MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)) -> PosBuilder
{-# INLINE mkPosBuilder #-}
mkPosBuilder sz f = PosBuilder (sz, oneShot $ \mar -> oneShot $ \n -> oneShot $ \s -> f mar n s)

instance Semigroup PosBuilder where
  PosBuilder (lSz, lF) <> PosBuilder (rSz, rF) = mkPosBuilder (lSz + rSz) $ \mar i s -> case lF mar i s of (# s', i' #) -> rF mar i' s'
  {-# INLINE (<>) #-}

instance Monoid PosBuilder where
  mempty = mkPosBuilder 0 $ \_ i s -> (# s, i #)
  {-# INLINE mempty #-}

pushInt :: Int -> PosBuilder
{-# INLINE pushInt #-}
pushInt (I# i) = mkPosBuilder 1 $ \mar ind s -> (# writeIntArray# mar ind i s, ind +# 1# #)

runPosBuilder :: PosBuilder -> PrimArray Int
{-# INLINE runPosBuilder #-}
runPosBuilder (PosBuilder (sz, f)) = runST $ do
  mar@(MutablePrimArray mar#) <- newPrimArray sz
  primitive $ \s -> (# case f mar# 0# s of (# s', _ #) -> s', () #)
  unsafeFreezePrimArray mar

-- Why 4 states?
-- Well it comes down to BitSets. A Not filter that matches components does not mean
-- Failure because the components may have BitSets, which would mean some entities may
-- still be matched by filter. So any filter that is not Failure is accepted, the BitSets
-- are checked before iteration

-- CPS because it makes ghc much more eager to inline, and thats what we need here
-- I need to make this represent multiple matches from wildcards
newtype FilterFunction = FilterFunction
  { runFilterFunction
      :: forall r
      . Archetype
      -> (PosBuilder -> r -> r)
      -> (PosBuilder -> r -> r)
      -> (PosBuilder -> r -> r)
      -> (PosBuilder -> r -> r)
      -> r
      -> r
  }

invert :: FilterFunction -> FilterFunction
{-# INLINE invert #-}
invert (FilterFunction g) = FilterFunction $ \aty aF iaF ifF fF ->
  g aty iaF aF fF ifF

and :: FilterFunction -> FilterFunction -> FilterFunction
{-# INLINE and #-}
and (FilterFunction f) (FilterFunction g) = FilterFunction $ \aty aF iaF ifF fF initial ->
  f aty
    (\p1 x1 -> g aty
      (\p2 x2 -> aF  (p1 <> p2) x2)
      (\p2 x2 -> iaF (p1 <> p2) x2)
      (\p2 x2 -> ifF (p1 <> p2) x2)
      (\p2 x2 -> fF  (p1 <> p2) x2)
      x1
      )
    (\p1 x1 -> g aty
      (\p2 x2 -> iaF (p1 <> p2) x2)
      (\p2 x2 -> iaF (p1 <> p2) x2)
      (\p2 x2 -> iaF (p1 <> p2) x2)
      (\p2 x2 -> fF  (p1 <> p2) x2)
      x1
      )
    (\p1 x1 -> g aty
      (\p2 x2 -> aF  (p1 <> p2) x2)
      (\p2 x2 -> iaF (p1 <> p2) x2)
      (\p2 x2 -> iaF (p1 <> p2) x2)
      (\p2 x2 -> fF  (p1 <> p2) x2)
      x1
      )
    (\p1 x1 -> g aty
      (\p2 x2 -> fF  (p1 <> p2) x2)
      (\p2 x2 -> fF  (p1 <> p2) x2)
      (\p2 x2 -> fF  (p1 <> p2) x2)
      (\p2 x2 -> fF  (p1 <> p2) x2)
      x1
      )
    initial

data Filter (hasMain :: Bool) ty where
  FilterWithMain :: ComponentId Any -> FilterFunction -> Filter 'True ty
  FilterNoMain   :: ComponentId Any -> FilterFunction -> Filter 'False ty

extractMain :: Filter 'True ty -> ComponentId Any
{-# INLINE extractMain #-}
extractMain (FilterWithMain c _) = coerce c

extractF :: Filter hasMain ty -> FilterFunction
{-# INLINE extractF #-}
extractF (FilterWithMain _ f) = f
extractF (FilterNoMain   _ f) = f

runFilter :: Filter hasMain ty -> Archetype -> (TypedArchetype ty -> r -> r) -> r -> r
{-# INLINE runFilter #-}
runFilter fi aty step initial = runFilterFunction f aty successF successF successF (\_ _ -> inline initial) initial
  where
    successF p x = step (TypedArchetype aty $ runPosBuilder p) x
    {-# INLINE successF #-}
    f = extractF fi

componentWithId :: forall {k} (c :: k) . Component c => ComponentId c -> Filter 'True c
{-# INLINE componentWithId #-}
componentWithId !cid
  | (coerce @_ @(Bitfield Int Entity) cid).tag.isRelation
    = FilterWithMain (coerce cid) $ FilterFunction $ \aty af _ _ ff initial ->
      Archetype.lookupRelationColumns aty (coerce cid)
        (\relId ind r -> af (pushInt relId <> pushInt ind) r)
        $ Archetype.lookupColumn aty cid
          (oneShot $ \i -> af (pushInt (coerce cid) <> pushInt i) initial)
          (ff (pushInt (coerce cid) <> pushInt (-1)) initial)
  | otherwise
    = FilterWithMain (coerce cid) $ FilterFunction $ \aty af _ _ ff initial ->
      Archetype.lookupColumn aty cid
        (oneShot $ \i -> af (pushInt (coerce cid) <> pushInt i) initial)
        (ff (pushInt (coerce cid) <> pushInt (-1)) initial)

data And a b

filterAnd :: forall lM l rM r . Filter lM l -> Filter rM r -> Filter (lM Bool.|| rM) (And l r)
{-# INLINE filterAnd #-}
filterAnd lFi rFi = con $ and f g
  where
    f = extractF lFi
    g = extractF rFi
    con = case (lFi, rFi) of
      (FilterWithMain c _, _                 ) -> FilterWithMain c
      (FilterNoMain   _ _, FilterWithMain c _) -> FilterWithMain c
      (FilterNoMain   c _, FilterNoMain   _ _) -> FilterNoMain c

data Not a

filterNot :: Filter hm a -> Filter (Bool.Not hm) (Not a)
{-# INLINE filterNot #-}
filterNot fi = con $ invert f
  where
    f = extractF fi
    con = case fi of
      FilterWithMain c _ -> FilterNoMain c
      FilterNoMain   c _ -> FilterWithMain c

data Up c a

filterUp :: Component c => ComponentId c -> Filter hasMain ty -> Filter 'True (Up c a)
{-# filterUp #-}
filterUp cid fi = FilterWithMain (coerce cid) $ FilterFunction $ \aty ->
  undefined


-- Filter Or and make column available as Either?
-- Filter Maybe
-- Allow filters without a main id, those just match everything
-- Sources in filters, fixed sources, attributes etc
-- Fixed sources in Folds are also neat, invisible to users but not to the compiler
-- Relationship traversals (https://www.flecs.dev/flecs/md_docs_Queries.html#autotoc_md239)
-- we can probably support variables with type families since we store the metadata anyway

-- Archetype that has passed a Filter ty
-- The first array contains the column indices in the order they appear in ty
data TypedArchetype ty
  = TypedArchetype
    {-# UNPACK #-} !Archetype
    {-# UNPACK #-} !(PrimArray Int)
    -- TODO Also store the component id we matched here (or just read it from the archetype given the constant time index from the ind array)
    -- But still provide as an api (in columns, as that has the type level machinery to match)

-- Perf:
-- This is usually called in an outer loop over matched archetypes (from either filters or queries)
-- As such it is quite annoying that ghc chooses to heap allocate the cont argument to getBitSet and also the
-- arguments to the closures produced by foldBitSets. This is usually fine because the inner loops are as good as
-- they can be, so as long as iterating a single archetype takes much longer than this outer loop step its fine,
-- but it would be nice if ghc produced less allocation heavy code here (also leads to more gc time...)
-- Things that worked:
-- - Storing unlifted stuff in queries. This eliminated one branch and a few stack moves from the outer loop
-- - Using oneShot for cont. I have no idea why this is faster, this causes the cont closure to be heap allocated.
--   Filters are slower without it but not by much, but queries get much slower without it
-- - Using a cps version of the BitSet
-- Things I tried and that failed:
-- - Using a non-cps version of BitSet. Slower for both filters and queries, and for some reason queries are now worse
--   than filters by about 1.5x
-- - Using an unboxed sum to represent the BitSet. Has exactly the same characteristics as the non-cps version, minus
--   a tiny amount of allocation
-- - Adding f and nrRows as an explicit parameter to goAll and goBitSet so that ghc doesn't need to allocate anything.
--   This turned out to be almost 10x slower for both filters and queries
-- Things that ghc misses:
-- - It doesn't inline BitSet fully
-- - If the same filter is used multiple times it also reuses it, which defeats the purpose of all the inlining
-- - (asm) It produces:
--     andl $1, %ebx
--     testq %rbx, %rbx
--     je lbl
--   Which completely misses the (existing) optimization of doing:
--     testq $1, %rbx
--     je lbl
--   Weirdly it does this correctly in the loop, but not in the inlined first iteration
-- TODO Make this a right fold instead
foldTypedArchetype :: forall ty z . FoldBitSets ty => TypedArchetype ty -> (Int -> z -> IO z) -> IO z -> IO z
{-# INLINE foldTypedArchetype #-}
foldTypedArchetype (TypedArchetype aty indices) f mz = do
  atyFlags <- Archetype.getFlags aty
  nrRows <- Archetype.getNrRows aty
  let goAll !n !z
        | n >= nrRows = pure z
        | otherwise = f n z >>= goAll (n + 1)
      goBitSet !arr !sz !n !z
        | n >= sz = pure z
        | otherwise = do
          w <- readPrimArray arr n
          goInner n w z >>= goBitSet arr sz (n + 1)
      goInner !_ 0 !z = pure z
      goInner !n w !z =
        let t = w .&. (- w)
            r = countTrailingZeros w
            w' = w `xor` t
            ind = n * 64 + r
        in if ind >= nrRows
          then pure z
          else f (n * 64 + r) z >>= goInner n w'
  initSt <- mz
  
  let getBitSet = \cont ->
        if get @"hasBitSets" atyFlags
          then foldBitSets @ty aty indices 0 $ \_ -> cont
          else cont $ mkBitSet $ \onFull _ _ _ -> onFull
  -- TODO This call to g is basically never inlined, code is still fast, but could be better
  -- The oneShot is necessary, otherwise ghc produces nicer looking core but is ultimately slower
  getBitSet $ oneShot $ \(BitSet g) -> g
    (goAll 0 initSt)
    (pure initSt)
    (\arr# -> do
      let arr = MutablePrimArray @_ @Word64 arr#
      -- TODO: This size computation is a signed quot 8. This needs the sign bit added, but this is
      --  always positive, so it would be better as an unsigned division as that is only the shift
      sz <- getSizeofMutablePrimArray arr
      goBitSet arr sz 0 initSt
      )
    (\arr# -> do
      let arr = MutablePrimArray @_ @Word64 arr#
      sz <- getSizeofMutablePrimArray arr
      goBitSet arr sz 0 initSt
      )

-- CompBitSet is an optimization that uses the components bitset. It is copied on write.
-- data BitSet s = Empty | Full | CompBitSet (MutablePrimArray s Word64) | BitSet (MutablePrimArray s Word64)

-- TODO Try this again with concrete types. But this likely fails just as the unboxed sum did
newtype BitSet = BitSet {
  runBitSet
    :: forall r
    .  IO r
    -> IO r
    -> (MutableByteArray# RealWorld -> IO r)
    -> (MutableByteArray# RealWorld -> IO r)
    -> IO r
    }

mkBitSet :: (forall r . IO r -> IO r -> (MutablePrimArray RealWorld Word64 -> IO r) -> (MutablePrimArray RealWorld Word64 -> IO r) -> IO r) -> BitSet
{-# INLINE mkBitSet #-}
mkBitSet = \g -> BitSet $ \onFull onEmpty onCopy onComp -> g onFull onEmpty (\(MutablePrimArray arr) -> onCopy arr) (\(MutablePrimArray arr) -> onComp arr)

class FoldBitSets ty where
  foldBitSets :: Archetype -> PrimArray Int -> Int -> (Int# -> BitSet -> IO r) -> IO r

instance (FoldBitSets l, FoldBitSets r) => FoldBitSets (And l r) where
  foldBitSets !aty !indices !n cont =
    foldBitSets @l aty indices n $ \n' (BitSet b1) ->
      foldBitSets @r aty indices (I# n') $ \n'' (BitSet b2) ->
        cont n'' $ mkBitSet $ \onFull onEmpty onCopy onComp ->
          b1
            (b2 onFull onEmpty (\arr -> onCopy $ MutablePrimArray arr) (\arr -> onComp $ MutablePrimArray arr))
            onEmpty
            (\arr1# ->
              let arr1 = MutablePrimArray arr1#
              in b2 (onCopy arr1) onEmpty
                (\arr2# ->
                  let arr2 = MutablePrimArray arr2#
                  in intersectionBitSet arr1 arr2 >> onCopy arr1
                  )
                (\arr2# ->
                  let arr2 = MutablePrimArray arr2#
                  in intersectionBitSet arr1 arr2 >> onCopy arr1
                  )
              )
            (\arr1# ->
              let arr1 = MutablePrimArray arr1#
              in b2 (onComp arr1) onEmpty
                (\arr2# ->
                  let arr2 = MutablePrimArray arr2#
                  in intersectionBitSet arr2 arr1 >> onCopy arr2
                  )
                (\arr2# -> do
                  let arr2 = MutablePrimArray arr2#
                  sz <- getSizeofMutablePrimArray arr1
                  mar1 <- newPrimArray sz
                  copyMutablePrimArray mar1 0 arr1 0 sz
                  intersectionBitSet mar1 arr2
                  onCopy mar1
                  )
              )
  {-# INLINE foldBitSets #-}

instance FoldBitSets a => FoldBitSets (Not a) where
  foldBitSets !aty !indices !n cont =
    foldBitSets @a aty indices n $ \n' (BitSet b) ->
      cont n' $ mkBitSet $ \onFull onEmpty onCopy _ ->
        b onEmpty onFull
          (\arr1# ->
            let arr1 = MutablePrimArray arr1#
            in complementBitSet arr1 >> onCopy arr1
            )
          (\arr1# -> do
            let arr1 = MutablePrimArray arr1#
            sz <- getSizeofMutablePrimArray arr1
            mar1 <- newPrimArray sz
            copyMutablePrimArray mar1 0 arr1 0 sz
            complementBitSet mar1
            onCopy mar1
            )
  {-# INLINE foldBitSets #-}

instance {-# OVERLAPPABLE #-} Component c => FoldBitSets c where
  foldBitSets !aty !indices !n = oneShot $ \cont ->
    let column = indexPrimArray indices (n * 2 + 1)
        !(I# next) = n + 1
    in if column /= -1
      then
        Archetype.getBitSet @(ComponentKindFor c) aty column
          (\arr -> cont next $ mkBitSet $ \_ _ _ onComp -> onComp arr)
          $ cont next $ mkBitSet $ \onFull _ _ _ -> onFull
      else cont next $ mkBitSet $ \_ onEmpty _ _ -> onEmpty
  {-# INLINE foldBitSets #-}

-- TODO I probably don't need it, but this could be simd ops

complementBitSet :: MutablePrimArray RealWorld Word64 -> IO ()
complementBitSet arr = do
  sz <- getSizeofMutablePrimArray arr
  let go n
       | n >= sz = pure ()
       | otherwise = do
        w <- readPrimArray arr n
        writePrimArray arr n $ complement w
        go (n + 1) 
  go 0

intersectionBitSet :: MutablePrimArray RealWorld Word64 -> MutablePrimArray RealWorld Word64 -> IO ()
intersectionBitSet dst src = do
  sz <- getSizeofMutablePrimArray dst
  let go n
       | n >= sz = pure ()
       | otherwise = do
        w1 <- readPrimArray dst n
        w2 <- readPrimArray src n
        writePrimArray dst n $ w1 .&. w2
        go (n + 1) 
  go 0
