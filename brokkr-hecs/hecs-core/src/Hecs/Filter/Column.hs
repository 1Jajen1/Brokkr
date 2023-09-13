{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Hecs.Filter.Column (
  withColumnMWithId
, withColumnWithId
, withColumn
, withEntityIdColumn
, withComponentId
, TypedHas
, FoldMatches, TypeMatches
, FirstMatch
) where

import Data.Coerce
import Data.Kind
import Data.Primitive
import Data.Proxy

import GHC.Exts (RealWorld)
import GHC.TypeLits

import Hecs.Archetype.Internal qualified as Archetype

import Hecs.Component.Column
import Hecs.Component.Internal

import Hecs.Filter.Internal

import Debug.Trace

-- | Retrieve a column with all 'EntityId's
withEntityIdColumn :: TypedArchetype ty -> (Column Flat RealWorld EntityId -> IO r) -> IO r
{-# INLINE withEntityIdColumn #-}
withEntityIdColumn (TypedArchetype aty _) cont =
  Archetype.getEntityColumn aty cont

-- | Lookup a column without any type level evidence that the column exists
--
-- If you have type level evidence, use 'withColumnWithId' or 'withColumn'
withColumnMWithId
  :: Component c
  => TypedArchetype ty
  -> ComponentId c
  -> (Column (ComponentKindFor c) RealWorld c -> IO r)
  -> IO r
  -> IO r
{-# INLINE withColumnMWithId #-}
withColumnMWithId (TypedArchetype aty _) compId onSucc onFail =
  Archetype.lookupColumn aty compId (\n -> Archetype.getColumn aty n onSucc) onFail

-- Bit of type level machinery to retrieve columns from filtered archetypes in constant time
-- (Or O(num matches) in case of multiple instances of the search type in the filter)

-- Type level failure if no proof can be found that a filter matched a component. Note: The inverse
-- of this is that the filter *may* have matched, not that it has!

-- | Retrieve a column that matches type 'c' and the 'ComponentId'
--
-- Useful if you have multiple dynamic components matched in a filter,
-- if you have only one occurrence of 'c' use 'withColumn' instead
withColumnWithId
  :: forall c r ty
  . ( TypedHas ty c
    , FoldMatches (TypeMatches ty c 0)
    , Component c
    )
  => TypedArchetype ty
  -> ComponentId c
  -> (Column (ComponentKindFor c) RealWorld c -> IO r)
  -> IO r
  -> IO r
{-# INLINE withColumnWithId #-}
withColumnWithId (TypedArchetype aty indices) cid onSucc onFail =
  foldMatch @(TypeMatches ty c 0) indices (coerce cid)
    (\n -> Archetype.getColumn aty n onSucc)
    onFail

-- | Retrieve a column that matches type 'c'
--
-- If you have multiple components matching 'c', this will
-- return the first one. Use 'withColumnWithId' to match
-- against a specific 'ComponentId' in that case!
withColumn :: forall c r ty n
  . ( TypedHas ty c
    , FirstMatch ty c (TypeMatches ty c 0) ~ n
    , Component c
    , KnownNat n
    )
  => TypedArchetype ty
  -> (Column (ComponentKindFor c) RealWorld c -> IO r)
  -> IO r
{-# INLINE withColumn #-}
withColumn (TypedArchetype aty _) onSucc =
  Archetype.getColumn @c aty n onSucc
  where n = fromIntegral $ natVal (Proxy @n)

withComponentId :: forall c ty r n
  . ( TypedHas ty c
    , FirstMatch ty c (TypeMatches ty c 0) ~ n
    , KnownNat n
    )
  => TypedArchetype ty -> (ComponentId c -> IO r) -> IO r
{-# INLINE withComponentId #-}
withComponentId (TypedArchetype _ inds) onSucc =
  onSucc (coerce $ indexPrimArray inds (n * 2))
  where n = fromIntegral $ natVal (Proxy @n)

type family TypedHas ty c :: Constraint where
  TypedHas ty c = TypedHasI ty c (() :: Constraint) (TypeError (Text "No evidence that archetype has column " :<>: ShowType c :<>: Text " from the filter " :<>: ShowType ty))

type family TypedHasI ty c (t :: k) (f :: k) :: k where
  TypedHasI c c t _ = t
  TypedHasI (And l r) c t f = TypedHasI l c t (TypedHasI r c t f)
  TypedHasI (Not a) c t f = TypedHasI a c f t
  TypedHasI a c _ f = f

type family TypeMatches ty c (n :: Nat) :: [(Bool, Nat)] where
  TypeMatches (And l r) c n = Concat (TypeMatches l c n) (TypeMatches r c (TypeSize l + n))
  TypeMatches (Not x) c n = InvertMatches (TypeMatches x c n)
  TypeMatches c c n = '[ '(True, n)]
  TypeMatches x c n = '[ '(False, n)]

type family InvertMatches (xs :: [(Bool,Nat)]) :: [(Bool,Nat)] where
  InvertMatches '[] = '[]
  InvertMatches ( '(True,n) : xs)  = '(False,n) : InvertMatches xs
  InvertMatches ( '(False,n) : xs) = '(True,n) : InvertMatches xs

type family TypeSize ty :: Nat where
  TypeSize (And l r) = TypeSize l + TypeSize r
  TypeSize (Not x) = TypeSize x
  TypeSize _ = 1

type family Concat (xs :: [k]) (ys :: [k]) :: [k] where
  Concat '[] ys = ys
  Concat (x:xs) ys = x : Concat xs ys

class FoldMatches (xs :: [(Bool, Nat)]) where
  foldMatch :: PrimArray Int -> Int -> (Int -> r) -> r -> r

instance FoldMatches '[] where
  foldMatch _ _ _ onFail = onFail
  {-# INLINE foldMatch #-}

instance (FoldMatches xs, KnownNat n) => FoldMatches ( '(True,n):xs) where
  foldMatch indices compId onSucc onFail
    | indexPrimArray indices (2 * ind) == compId
      = onSucc $ indexPrimArray indices (1 + 2 * ind)
    | otherwise
      = foldMatch @xs indices compId onSucc onFail
    where
      ind = fromIntegral $ natVal (Proxy @n)
  {-# INLINE foldMatch #-}

instance FoldMatches xs => FoldMatches ( '(False,n):xs) where
  foldMatch indices compId onSucc onFail
    = foldMatch @xs indices compId onSucc onFail
  {-# INLINE foldMatch #-}

type family FirstMatch ty c (xs :: [(Bool, Nat)]) :: Nat where
  FirstMatch ty c '[] = TypeError (Text "No evidence that archetype has column " :<>: ShowType c :<>: Text " from the filter " :<>: ShowType ty)
  FirstMatch ty c ( '(False, n) : xs) = FirstMatch ty c xs
  FirstMatch _  _ ( '(True, n) : xs) = n
