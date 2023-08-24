{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main (main) where

import Brokkr.HashTable

import Test.Tasty.Bench
import System.Random (mkStdGen, randomRs)
import Control.DeepSeq
import Prelude hiding (lookup)
import Data.IntSet qualified as IS
import Data.HashTable.IO qualified as HT
import Data.HashTable.ST.Basic qualified as HTB
import Data.HashTable.ST.Linear qualified as HTL
import Data.HashTable.ST.Cuckoo qualified as HTC
import Data.Vector.Hashtables qualified as VHT
import Data.Vector.Mutable qualified as MV
import Data.Vector.Storable.Mutable qualified as SMV
import Data.Vector.Primitive.Mutable qualified as PMV
import GHC.Exts (RealWorld)
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as V
import Data.Hashable qualified as HS
import Foreign.Storable qualified as Storable
import Data.Vector.Primitive qualified as Prim

numIterations = 1_000

main :: IO ()
main = do
  defaultMain $ [
      benchHashTable "Int (random)" V.mapM_ numEls (V.fromList keys) (V.fromList $ take numIterations $ filter (\i -> not $ IS.member i keySet) $ rnd' maxBound)
    | n0 <- take 6 [2..], (n1,n2) <- zip [3,1,1,1,1,0] [4,2,3,4,8,1]
      , let n = 10 ^ (n0 :: Int); numEls = n - (n * n1) `quot` n2; keys = take numEls $ rnd maxBound; keySet = IS.fromList keys
    ]

-- These NFData instances are a lie, but we don't need to force values for anything and keys are usually forced anyway
instance NFData (HashTable' ks vs k v s) where
  rnf !_ = ()
instance NFData (VHT.Dictionary s ks k vs v) where
  rnf !_ = ()
instance NFData (HTB.HashTable RealWorld k v) where
  rnf !_ = ()
instance NFData (HTL.HashTable RealWorld k v) where
  rnf !_ = ()
instance NFData (HTC.HashTable RealWorld k v) where
  rnf !_ = ()
instance Hash Int where
  hash i = HashFn (const i)

benchHashTable :: forall vec key .
  ( NFData (vec key), VG.Vector vec key
  , Show key, Hash key, HS.Hashable key, Storable.Storable key
  , Prim.Prim key
  ) => String -> ((key -> IO ()) -> vec key -> IO ()) -> Int -> vec key -> vec key -> Benchmark
{-# INLINE benchHashTable #-}
benchHashTable name traverse_ sz keys others = bgroup name
  [ bgroup ("Initial table size: " <> show sz) $
    [ bgroup "brokkr-hashtables"
      [ benchOne @Boxed @Boxed "BB"
      , benchOne @Boxed @Storable "BS"
      , benchOne @Storable @Boxed "SB"
      , benchOne @Storable @Storable "SS"
      , benchOne @Boxed @Prim "BP"
      , benchOne @Prim @Boxed "PB"
      , benchOne @Prim @Prim "PP"
      , benchOne @Prim @Storable "PS"
      , benchOne @Storable @Prim "SP"
      ]
    , bgroup "hashtables"
      -- TODO add others here
      [ benchHashTables (HT.new @HTB.HashTable) "Basic"
      , benchHashTables (HT.new @HTL.HashTable) "Linear"
      , benchHashTables (HT.new @HTC.HashTable) "Cuckoo"
      ]
    , bgroup "vector-hashtables"
      [ benchVectorHashTables (VHT.initialize @MV.MVector @key @MV.MVector @key 32) "BB"
      , benchVectorHashTables (VHT.initialize @MV.MVector @key @SMV.MVector @key 32) "BS"
      , benchVectorHashTables (VHT.initialize @SMV.MVector @key @MV.MVector @key 32) "SB"
      , benchVectorHashTables (VHT.initialize @SMV.MVector @key @SMV.MVector @key 32) "SS"
      , benchVectorHashTables (VHT.initialize @MV.MVector @key @PMV.MVector @key 32) "BP"
      , benchVectorHashTables (VHT.initialize @PMV.MVector @key @MV.MVector @key 32) "PB"
      , benchVectorHashTables (VHT.initialize @PMV.MVector @key @PMV.MVector @key 32) "PP"
      , benchVectorHashTables (VHT.initialize @PMV.MVector @key @SMV.MVector @key 32) "PS"
      , benchVectorHashTables (VHT.initialize @SMV.MVector @key @PMV.MVector @key 32) "SP"
      ]
    ]
  ]
  where
    valid :: vec key = VG.fromList $ take numIterations $ cycle $ VG.toList keys
    benchOne :: forall kS vS . HashTable kS vS key key => String -> Benchmark
    {-# INLINE benchOne #-}
    benchOne nm = bgroup nm
        [ bgroup "insert"
          [ env (pure keys) $ \ks -> bench "baseline" $ whnfIO $ do
              _ <- new @kS @vS @key @key 0 0.75
              traverse_ (\(!_) -> pure ()) ks
          , env (pure keys) $ \ks -> bench "baseline (reserve)" $ whnfIO $ do
              t <- new @kS @vS @key @key 0 0.75
              reserve t sz
              traverse_ (\(!_) -> pure ()) ks
          , env (pure keys) $ \ks -> bench "insert" $ whnfIO $ do
              t <- new @kS @vS @key @key 0 0.75
              traverse_ (\k -> insert t k k) ks
          , env (pure keys) $ \ks -> bench "insert (new|reserve)" $ whnfIO $ do
              t <- new @kS @vS @key @key 0 0.75
              reserve t sz
              traverse_ (\k -> insert t k k) ks
          ]
        , bgroup "insert (maxLoadFactor = 0.9)"
          [ env (pure keys) $ \ks -> bench "baseline" $ whnfIO $ do
              _ <- new @kS @vS @key @key 0 0.9
              traverse_ (\(!_) -> pure ()) ks
          , env (pure keys) $ \ks -> bench "baseline (reserve)" $ whnfIO $ do
              t <- new @kS @vS @key @key 0 0.9
              reserve t sz
              traverse_ (\(!_) -> pure ()) ks
          , env (pure keys) $ \ks -> bench "insert" $ whnfIO $ do
              t <- new @kS @vS @key @key 0 0.9
              traverse_ (\k -> insert t k k) ks
          , env (pure keys) $ \ks -> bench "insert (new|reserve)" $ whnfIO $ do
              t <- new @kS @vS @key @key 0 0.9
              reserve t sz
              traverse_ (\k -> insert t k k) ks
          ]
        , bgroup "lookup"
          [ env (new @kS @vS @key @key 0 0.75 >>= \t -> traverse_ (\k -> insert t k k) keys >> pure (t, keys, valid))
              $ \ ~(t, _, xs) -> bench "lookup (success)" $ whnfIO $ do
                traverse_ (\k -> lookup t k >>= \case Nothing -> error (show k); Just _ -> pure ()) xs
          , env (new @kS @vS @key @key 0 0.75 >>= \t -> traverse_ (\k -> insert t k k) keys >> pure (t, keys, others))
              $ \ ~(t, _, xs) -> bench "lookup (fail)" $ whnfIO $ do
                traverse_ (\k -> lookup t k >>= \case Nothing -> pure (); Just _ -> error (show k)) xs
          ]
        , bgroup "lookup (maxLoadFactor = 0.9)"
          [ env (new @kS @vS @key @key 0 0.9 >>= \t -> traverse_ (\k -> insert t k k) keys >> pure (t, keys, valid))
              $ \ ~(t, _, xs) -> bench "lookup (success)" $ whnfIO $ do
                traverse_ (\k -> lookup t k >>= \case Nothing -> error (show k); Just _ -> pure ()) xs
          , env (new @kS @vS @key @key 0 0.9 >>= \t -> traverse_ (\k -> insert t k k) keys >> pure (t, keys, others))
              $ \ ~(t, _, xs) -> bench "lookup (fail)" $ whnfIO $ do
                traverse_ (\k -> lookup t k >>= \case Nothing -> pure (); Just _ -> error (show k)) xs
          ]
        ]
    {-# INLINE benchHashTables #-}
    benchHashTables initTable nm = bgroup nm
      [ bgroup "insert"
        [ env (pure keys) $ \ks -> bench "baseline" $ whnfIO $ do
            _ <- initTable
            traverse_ (\(!_) -> pure ()) ks
        , env (pure keys) $ \ks-> bench "insert" $ whnfIO $ do
            t <- initTable
            traverse_ (\k -> HT.insert t k k) ks
        ]
        , bgroup "lookup"
          [ env (HT.newSized 32 >>= \(t :: HT.BasicHashTable key key) -> traverse_ (\k -> HT.insert t k k) keys >> pure (t, keys, valid))
              $ \ ~(t, _, xs) -> bench "lookup (success)" $ whnfIO $ do
                traverse_ (\k -> HT.lookup t k >>= \case Nothing -> error (show k); Just _ -> pure ()) xs
          , env (HT.newSized 32 >>= \(t :: HT.BasicHashTable key key) -> traverse_ (\k -> HT.insert t k k) keys >> pure (t, keys, others))
              $ \ ~(t, _, xs) -> bench "lookup (fail)" $ whnfIO $ do
                traverse_ (\k -> HT.lookup t k >>= \case Nothing -> pure (); Just _ -> error (show k)) xs
          ]
      ]
    {-# INLINE benchVectorHashTables #-}
    benchVectorHashTables initTable nm = bgroup nm
      [ bgroup "insert"
        [ env (pure keys) $ \ks -> bench "baseline" $ whnfIO $ do
            _ <- initTable
            traverse_ (\(!_) -> pure ()) ks
        , env (pure keys) $ \ks -> bench "insert" $ whnfIO $ do
            t <- initTable
            traverse_ (\k -> VHT.insert t k k) ks
        ]
      , bgroup "lookup"
        [ env (initTable >>= \t -> traverse_ (\k -> VHT.insert t k k) keys >> pure (t, keys, valid))
            $ \ ~(t, _, xs) -> bench "lookup (success)" $ whnfIO $ do
              traverse_ (\k -> VHT.lookup t k >>= \case Nothing -> error (show k); Just _ -> pure ()) xs
        , env (initTable >>= \t -> traverse_ (\k -> VHT.insert t k k) keys >> pure (t, keys, others))
            $ \ ~(t, _, xs) -> bench "lookup (fail)" $ whnfIO $ do
              traverse_ (\k -> VHT.lookup t k >>= \case Nothing -> pure (); Just _ -> error (show k)) xs
        ]
      ]

rnd, rnd' :: Int -> [Int]
rnd upper = randomRs (0, upper) $ mkStdGen 1234
rnd' upper = randomRs (0, upper) $ mkStdGen 4321
