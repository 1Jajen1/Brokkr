{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main (main) where

import Brokkr.HashTable

import Criterion.Main
import System.Random (mkStdGen, randomRs)
import Control.DeepSeq
import Prelude hiding (lookup)
import Data.IntSet qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.HashMap.Strict qualified as HM
import Data.HashTable.IO qualified as HT
import Data.HashTable.ST.Basic qualified as HTB
import Data.HashTable.ST.Linear qualified as HTL
import Data.HashTable.ST.Cuckoo qualified as HTC
import Data.Vector.Hashtables qualified as VHT
import Data.Vector.Mutable qualified as MV
import Data.Vector.Storable.Mutable qualified as SMV
-- Have to use unboxed here because vector-hashtables forgot prim exist when writing instances for DeleteEntry
import Data.Vector.Unboxed.Mutable qualified as PMV
import GHC.Exts (RealWorld)
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as V
import Data.Hashable qualified as HS
import Foreign.Storable qualified as Storable
import Data.Vector.Primitive qualified as Prim
import Control.Monad (void)

numIterations = 1_000

main :: IO ()
main = do
  defaultMain $ [
      benchHashTable "Int (random)" (\l _ -> l) V.foldr V.mapM_ numEls (V.fromList keys) (V.fromList $ take numIterations $ filter (\i -> not $ IS.member i keySet) $ rnd' maxBound)
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
  , Show key, Hash key, HS.Hashable key
  , Storable.Storable key, Prim.Prim key, PMV.Unbox key -- TODO Conditional!
  , NFData key
  ) => String -> (forall r . (key ~ Int => r) -> r -> r) -> (forall z . (key -> z -> z) -> z -> vec key -> z) -> ((key -> IO ()) -> vec key -> IO ()) -> Int -> vec key -> vec key -> Benchmark
{-# INLINE benchHashTable #-}
benchHashTable name ifInt foldVec traverse_ sz keys others = bgroup name
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
    , bgroup "HashMap"
      [ env (pure keys) $ \ks -> bench "baseline" $ whnfIO $ traverse_ (\(!_) -> pure ()) ks
      , env (pure keys) $ \ks -> bench "insert" $ whnf (foldVec (\k x -> HM.insert k k x) HM.empty) ks
      , env (pure (foldVec (\k x -> HM.insert k k x) HM.empty keys, valid)) $ \ ~(im, xs) ->
        bench "lookup (success)" $ whnf (foldVec (\k () -> case HM.lookup k im of Nothing -> error (show k); Just _ -> ()) ()) xs
      , env (pure (foldVec (\k x -> HM.insert k k x) HM.empty keys, others)) $ \ ~(im, xs) ->
        bench "lookup (fail)" $ whnf (foldVec (\k () -> case HM.lookup k im of Nothing -> (); Just _ -> error (show k)) ()) xs
      , env (pure (foldVec (\k x -> HM.insert k k x) HM.empty keys, validOthers))
          $ \ ~(t, vo') -> bench "insert + delete" $ whnf (\vo ->
            let go1 !n !acc
                  | n >= VG.length vo = acc
                  | otherwise = do
                    let d = VG.unsafeIndex vo n
                        k = VG.unsafeIndex vo $ n + 1
                    go1 (n + 2) (HM.insert k k $ HM.delete d acc)
                t' = go1 0 t
                -- Run again but with the ops and order reversed, should restore the table exactly
                go2 !n !acc
                  | n < 0 = acc
                  | otherwise = do
                    let d = VG.unsafeIndex vo $ n + 1
                        k = VG.unsafeIndex vo n
                    go2 (n - 2) (HM.insert k k $ HM.delete d acc)
            in go2 (VG.length vo - 2) t') vo'
      ]
    ] <> (
      ifInt
        ( pure $ bgroup "IntMap"
            [ env (pure keys) $ \ks -> bench "baseline" $ whnfIO $ traverse_ (\(!_) -> pure ()) ks
            , env (pure keys) $ \ks -> bench "insert" $ whnf (foldVec (\k x -> IM.insert k k x) IM.empty) ks
            , env (pure (foldVec (\k x -> IM.insert k k x) IM.empty keys, valid)) $ \ ~(im, xs) ->
              bench "lookup (success)" $ whnf (foldVec (\k () -> case IM.lookup k im of Nothing -> error (show k); Just _ -> ()) ()) xs
            , env (pure (foldVec (\k x -> IM.insert k k x) IM.empty keys, others)) $ \ ~(im, xs) ->
              bench "lookup (fail)" $ whnf (foldVec (\k () -> case IM.lookup k im of Nothing -> (); Just _ -> error (show k)) ()) xs
            , env (pure (foldVec (\k x -> IM.insert k k x) IM.empty keys, validOthers))
                $ \ ~(t, vo') -> bench "insert + delete" $ whnf (\vo ->
                  let go1 !n !acc
                        | n >= VG.length vo = acc
                        | otherwise = do
                          let d = VG.unsafeIndex vo n
                              k = VG.unsafeIndex vo $ n + 1
                          go1 (n + 2) (IM.insert k k $ IM.delete d acc)
                      t' = go1 0 t
                      -- Run again but with the ops and order reversed, should restore the table exactly
                      go2 !n !acc
                        | n < 0 = acc
                        | otherwise = do
                          let d = VG.unsafeIndex vo $ n + 1
                              k = VG.unsafeIndex vo n
                          go2 (n - 2) (IM.insert k k $ IM.delete d acc)
                  in go2 (VG.length vo - 2) t') vo'
            ]
        )
        [])
  ]
  where
    valid :: vec key = VG.fromList $ take numIterations $ cycle $ VG.toList keys
    validOthers :: vec key = VG.fromList $ take (numIterations * 2) $ go (VG.toList keys) (VG.toList others)
      where
        go l r = goL l r []
        goL _ [] _ = error "Took too many"
        goL [] r acc = goL (reverse acc) r []
        goL (x:xs) (y:ys) acc = x : y : goL xs ys (y:acc)
    benchOne :: forall kS vS . HashTable kS vS key key => String -> Benchmark
    {-# INLINE benchOne #-}
    benchOne nm = bgroup nm
        [ bgroup "insert"
          [ bench "baseline" $ whnfIO $ do
              void $ new @kS @vS @key @key 0 0.75
          , bench "baseline (reserve)" $ whnfIO $ do
              t <- new @kS @vS @key @key 0 0.75
              reserve t sz
          , env (pure keys) $ \ks -> bench "insert" $ whnfIO $ do
              t <- new @kS @vS @key @key 0 0.75
              traverse_ (\k -> insert t k k) ks
          , env (pure keys) $ \ks -> bench "insert (new|reserve)" $ whnfIO $ do
              t <- new @kS @vS @key @key 0 0.75
              reserve t sz
              traverse_ (\k -> insert t k k) ks
          ]
        , bgroup "insert (maxLoadFactor = 0.9)"
          [ bench "baseline" $ whnfIO $ do
              void $ new @kS @vS @key @key 0 0.9
          , bench "baseline (reserve)" $ whnfIO $ do
              t <- new @kS @vS @key @key 0 0.9
              reserve t sz
          , env (pure keys) $ \ks -> bench "insert" $ whnfIO $ do
              t <- new @kS @vS @key @key 0 0.9
              traverse_ (\k -> insert t k k) ks
          , env (pure keys) $ \ks -> bench "insert (new|reserve)" $ whnfIO $ do
              t <- new @kS @vS @key @key 0 0.9
              reserve t sz
              traverse_ (\k -> insert t k k) ks
          ]
        , env (new @kS @vS @key @key 0 0.75 >>= \t0 -> reserve t0 sz >> traverse_ (\k -> insert t0 k k) keys >> pure (t0, validOthers))
            $ \ ~(t, vo) -> bench "insert + delete" $ whnfIO $ do
              let go1 n
                    | n >= VG.length vo = pure ()
                    | otherwise = do
                      delete t (VG.unsafeIndex vo n)
                      let k = VG.unsafeIndex vo $ n + 1
                      insert t k k
                      go1 $ n + 2
              go1 0
              -- Run again but with the ops and order reversed, should restore the table exactly
              let go2 n
                    | n < 0 = pure ()
                    | otherwise = do
                      delete t (VG.unsafeIndex vo $ n + 1)
                      let k = VG.unsafeIndex vo n
                      insert t k k
                      go1 $ n - 2
              go2 (VG.length vo - 2)
        , env (new @kS @vS @key @key 0 0.75 >>= \t0 -> reserve t0 sz >> traverse_ (\k -> insert t0 k k) keys >> pure (t0, validOthers))
            $ \ ~(t, vo) -> bench "insert + delete (maxLoadFactor = 0.9)" $ whnfIO $ do
              let go1 n
                    | n >= VG.length vo = pure ()
                    | otherwise = do
                      delete t (VG.unsafeIndex vo n)
                      let k = VG.unsafeIndex vo $ n + 1
                      insert t k k
                      go1 $ n + 2
              go1 0
              -- Run again but with the ops and order reversed, should restore the table exactly
              let go2 n
                    | n < 0 = pure ()
                    | otherwise = do
                      delete t (VG.unsafeIndex vo $ n + 1)
                      let k = VG.unsafeIndex vo n
                      insert t k k
                      go1 $ n - 2
              go2 (VG.length vo - 2)
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
        [ bench "baseline" $ whnfIO $ do
            void $ initTable
        , env (pure keys) $ \ks-> bench "insert" $ whnfIO $ do
            t <- initTable
            traverse_ (\k -> HT.insert t k k) ks
        ]
        , env (initTable >>= \t0 -> traverse_ (\k -> HT.insert t0 k k) keys >> pure (t0, validOthers))
            $ \ ~(t, vo) -> bench "insert + delete" $ whnfIO $ do
              let go1 n
                    | n >= VG.length vo = pure ()
                    | otherwise = do
                      HT.delete t (VG.unsafeIndex vo n)
                      let k = VG.unsafeIndex vo $ n + 1
                      HT.insert t k k
                      go1 $ n + 2
              go1 0
              -- Run again but with the ops and order reversed, should restore the table exactly
              let go2 n
                    | n < 0 = pure ()
                    | otherwise = do
                      HT.delete t (VG.unsafeIndex vo $ n + 1)
                      let k = VG.unsafeIndex vo n
                      HT.insert t k k
                      go1 $ n - 2
              go2 (VG.length vo - 2)
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
        [ bench "baseline" $ whnfIO $ do
           void $ initTable
        , env (pure keys) $ \ks -> bench "insert" $ whnfIO $ do
            t <- initTable
            traverse_ (\k -> VHT.insert t k k) ks
        ]
      , env (initTable >>= \t0 -> traverse_ (\k -> VHT.insert t0 k k) keys >> pure (t0, validOthers))
          $ \ ~(t, vo) -> bench "insert + delete" $ whnfIO $ do
            let go1 n
                  | n >= VG.length vo = pure ()
                  | otherwise = do
                    VHT.delete t (VG.unsafeIndex vo n)
                    let k = VG.unsafeIndex vo $ n + 1
                    VHT.insert t k k
                    go1 $ n + 2
            go1 0
            -- Run again but with the ops and order reversed, should restore the table exactly
            let go2 n
                  | n < 0 = pure ()
                  | otherwise = do
                    VHT.delete t (VG.unsafeIndex vo $ n + 1)
                    let k = VG.unsafeIndex vo n
                    VHT.insert t k k
                    go1 $ n - 2
            go2 (VG.length vo - 2)
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
