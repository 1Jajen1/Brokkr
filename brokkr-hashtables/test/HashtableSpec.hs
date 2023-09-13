{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
module HashtableSpec (spec) where

import Test.Syd
import Test.Syd.Hedgehog ()

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Brokkr.HashTable qualified as HT
import Data.Coerce

import Foreign.Storable qualified as Storable
import Data.Primitive qualified as Prim

import Data.Foldable (traverse_)
import Data.Containers.ListUtils (nubOrdOn)

spec :: Spec
spec = do
  describe "BB" $ tableSpec @HT.Boxed @HT.Boxed (RandInt <$> Gen.int Range.constantBounded) (Gen.int Range.constantBounded)
  describe "SB" $ tableSpec @HT.Storable @HT.Boxed (RandInt <$> Gen.int Range.constantBounded) (Gen.int Range.constantBounded)
  describe "BS" $ tableSpec @HT.Boxed @HT.Storable (RandInt <$> Gen.int Range.constantBounded) (Gen.int Range.constantBounded)
  describe "SS" $ tableSpec @HT.Storable @HT.Storable (RandInt <$> Gen.int Range.constantBounded) (Gen.int Range.constantBounded)
  describe "PB" $ tableSpec @HT.Prim @HT.Boxed (RandInt <$> Gen.int Range.constantBounded) (Gen.int Range.constantBounded)
  describe "BP" $ tableSpec @HT.Boxed @HT.Prim (RandInt <$> Gen.int Range.constantBounded) (Gen.int Range.constantBounded)
  describe "PP" $ tableSpec @HT.Prim @HT.Prim (RandInt <$> Gen.int Range.constantBounded) (Gen.int Range.constantBounded)
  describe "SP" $ tableSpec @HT.Storable @HT.Prim (RandInt <$> Gen.int Range.constantBounded) (Gen.int Range.constantBounded)
  describe "PS" $ tableSpec @HT.Prim @HT.Storable (RandInt <$> Gen.int Range.constantBounded) (Gen.int Range.constantBounded)

newtype RandInt = RandInt Int
  deriving newtype (Eq, Ord, Show, Num, Storable.Storable, Prim.Prim)

instance HT.Hash RandInt where
  hash (RandInt x) = HT.HashFn (const x)

-- Initially used for testing that the correct salt is used,
-- but now also serves as a good source of collisions
newtype ConstSalt k = ConstSalt k
  deriving newtype (Eq, Show, Storable.Storable, Prim.Prim)

instance HT.Hash (ConstSalt k) where
  hash _ = HT.HashFn id

tableSpec :: forall keyStorage valueStorage k v .
  ( HT.HashTable keyStorage valueStorage (ConstSalt k) v
  , HT.HashTable keyStorage valueStorage k v
  , Eq k, Ord k, HT.Hash k, Show k
  , Eq v, Show v
  ) => Gen k -> Gen v -> Spec
tableSpec genK genV = do
  describe "new" $ do
    it "should use the correct salt" $ property $ do
      s <- forAll $ Gen.int Range.constantBounded
      (k,v) <- forAll ((,) <$> genK <*> genV)
      table <- HT.new @keyStorage @valueStorage @(ConstSalt k) @v 32 s 0.75
      HT.insert table (ConstSalt k) v
      let kHash = coerce (HT.hash (ConstSalt k)) s
      HT.lookupWithHash table (ConstSalt k) kHash
        (\v' -> v' === v)
        failure
  describe "grow" $ do
    it "should handle a lot of colliding keys getting inserted" $ property $ do
      s <- forAll $ Gen.int Range.constantBounded
      -- This inserts elements that always collide. Since we grow as soon
      -- as we exceed max distance of log_2(capacity), inserting 20 colliding
      -- elements forces a hashtable size of 2^20 ~ 1mil
      -- This is obviously rare with any decent hash function (outside of dos attacks)
      -- For protection against dos use an even better hash
      xs0 <- forAll $ Gen.list (Range.linear 5 20) ((,) <$> genK <*> genV)
      let xs = nubOrdOn fst xs0
      table <- HT.new @keyStorage @valueStorage @(ConstSalt k) @v 32 s 0.75
      traverse_ (\(k,v) -> HT.insert table (ConstSalt k) v) xs
      -- check that the table growth worked and we still have our elements
      traverse_ (\(k,v) -> HT.lookup table (ConstSalt k)
        (\v' -> v' === v)
        failure
        ) xs
    it "should handle the load factor being exceeded" $ property $ do
      s <- forAll $ Gen.int Range.constantBounded
      -- Currently table min capacity is 32. Change the min here to force growth
      xs0 <- forAll $ Gen.list (Range.linear 32 5000) ((,) <$> genK <*> genV)
      let xs = nubOrdOn fst xs0
      table <- HT.new @keyStorage @valueStorage @k @v 32 s 0.75
      traverse_ (uncurry $ HT.insert table) xs
      -- check that the table growth worked and we still have our elements
      traverse_ (\(k,v) -> HT.lookup table k
        (\v' -> v' === v)
        failure
        ) xs
  describe "reserve" $ do
    it "should still contain all previously inserted keys" $ property $ do
      s <- forAll $ Gen.int Range.constantBounded
      xs0 <- forAll $ Gen.list (Range.linear 0 100) ((,) <$> genK <*> genV)
      let xs = nubOrdOn fst xs0
      table <- HT.new @keyStorage @valueStorage @k @v 32 s 0.75
      traverse_ (uncurry $ HT.insert table) xs
      -- reserve n must be called such that
      -- nextPowerOf2(n / loadFactor) > nextPowerOf2(length xs / loadFactor)
      -- otherwise no resize will happen
      -- And even still: If there are too many collisions on one hash the
      -- table can still grow to that size. But since this is a property
      -- test, most runs should not hit that rare case where reserve does
      -- not grow
      HT.reserve table 20_000
      -- check that the table resize worked and we still have our elements
      traverse_ (\(k,v) -> HT.lookup table k
        (\v' -> v' === v)
        failure
        ) xs
  describe "lookup/insert" $ do
    it "should be able to lookup a previously inserted key" $ property $ do
      (k,v) <- forAll ((,) <$> genK <*> genV)
      table <- HT.new @keyStorage @valueStorage @k @v 32 0 0.75
      HT.insert table k v
      HT.lookup table k
        (\v' -> v' === v)
        failure
    it "should be able to lookup a previously inserted key (multiple)" $ property $ do
      kvs0 <- forAll $ Gen.list (Range.linear 1 1_000) ((,) <$> genK <*> genV)
      let kvs = nubOrdOn fst kvs0
      table <- HT.new @keyStorage @valueStorage @k @v 32 0 0.75
      traverse_ (uncurry (HT.insert table)) kvs
      traverse_ (\(k,v) -> HT.lookup table k
        (\v' -> v' === v)
        $ annotate (show k) >> failure
        ) kvs
    it "should be able to handle lookup a previously inserted key which collides with another" $ property $ do
      table <- HT.new @keyStorage @valueStorage @(ConstSalt k) @v 32 0 0.75
      (k1,k2,v1,v2) <- forAll ((,,,) <$> genK <*> genK <*> genV <*> genV)
      HT.insert table (ConstSalt k1) v1
      HT.insert table (ConstSalt k2) v2
      HT.lookup table (ConstSalt k1)
        (\v' -> v' === v1)
        failure
      HT.lookup table (ConstSalt k2)
        (\v' -> v' === v2)
        failure
    it "should not be able to lookup a not inserted key" $ property $ do
      k <- forAll genK
      table <- HT.new @keyStorage @valueStorage @k @v 32 0 0.75
      HT.lookup table k
        (\_ -> failure)
        $ pure ()
    it "should not be able to handle lookup a not inserted key which collides with another inserted key" $ property $ do
      (k1,k2,v1) <- forAll ((,,) <$> genK <*> genK <*> genV)
      table <- HT.new @keyStorage @valueStorage @(ConstSalt k) @v 32 0 0.75
      HT.insert table (ConstSalt k1) v1
      HT.lookup table (ConstSalt k1)
        (\v' -> v' === v1)
        failure
      HT.lookup table (ConstSalt k2)
        (\_ -> failure)
        $ pure ()
    it "should not be able to lookup a deleted inserted key" $ property $ do
      (k,v) <- forAll ((,) <$> genK <*> genV)
      table <- HT.new @keyStorage @valueStorage @k @v 32 0 0.75
      HT.insert table k v
      HT.lookup table k
        (\v' -> v' === v)
        failure
      _ <- HT.delete table k
      HT.lookup table k
        (\_ -> failure)
        $ pure ()
    it "should not be able to handle lookup a not inserted key which collides with another inserted key" $ property $ do
      table <- HT.new @keyStorage @valueStorage @(ConstSalt k) @v 32 0 0.75
      (k1,k2,v1) <- forAll ((,,) <$> genK <*> genK <*> genV)
      HT.insert table (ConstSalt k1) v1
      HT.lookup table (ConstSalt k1)
        (\v' -> v' === v1)
        failure
      HT.lookup table (ConstSalt k2)
        (\_ -> failure)
        $ pure ()
      _ <- HT.delete table (ConstSalt k1)
      HT.lookup table (ConstSalt k1)
        (\_ -> failure)
        $ pure ()
      HT.lookup table (ConstSalt k2)
        (\_ -> failure)
        $ pure ()
