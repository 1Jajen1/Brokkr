{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

import Test.Tasty.Bench

import Brokkr.BlockState.Internal.Conversion

import Control.DeepSeq

import Data.ByteString (ByteString)

import Data.Proxy

import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U

import GHC.Generics (Generic)
import GHC.TypeLits

import System.Random

main :: IO ()
main = defaultMain [
    env setupEnv $ \ ~(Env{..}) ->
    bench "(all) (ByteString, [(ByteString, ByteString)]) -> BlockState"
      $ nf (V.foldl' (\() (n, props) -> rnf $ propsToId n props (hashProps n props)) ()) allEntries
  , env setupEnv $ \ ~(Env{..}) ->
    bench "(all) BlockState -> (ByteString, [(ByteString, ByteString)])"
      $ nf (U.foldl' (\() x -> rnf $ idToProps x) ()) allStates
  -- The following are random (fixed seed) lists of stuff to lookup. This simulates real world access patterns better
  -- and minimizes the effect of a cache
  , env setupEnv $ \ ~(Env{..}) ->
    bench "(all) (ByteString, [(ByteString, ByteString)]) -> BlockState (random)"
      $ nf (V.foldl' (\() (n, props) -> rnf $ propsToId n props (hashProps n props)) ()) randEntries
  , env setupEnv $ \ ~(Env{..}) ->
    bench "(all) BlockState -> (ByteString, [(ByteString, ByteString)]) (random)"
      $ nf (U.foldl' (\() x -> rnf $ idToProps x) ()) randStates
  -- 
  , env setupEnv $ \ ~(Env{..}) ->
    bench "(one) (ByteString, [(ByteString, ByteString)]) -> BlockState (random)"
      $ nf (\(n, props) -> propsToId n props (hashProps n props)) (V.head randEntries)
  , env setupEnv $ \ ~(Env{..}) ->
    bench "(one) BlockState -> (ByteString, [(ByteString, ByteString)]) (random)"
      $ nf idToProps (U.head randStates)
  -- bench the hash function
  -- (all) takes much much longer than I expected, I guess that is memory load cost
  -- and (one) remains in cache for the entire benchmark
  , env setupEnv $ \ ~(Env{..}) ->
    bench "(all) (ByteString, [(ByteString, ByteString)]) -> Hash"
      $ nf (V.foldl' (\() (n, props) -> rnf $ hashProps n props) ()) allEntries
  , env setupEnv $ \ ~(Env{..}) ->
    bench "(one) (ByteString, [(ByteString, ByteString)]) -> Hash"
      $ nf (\(n, props) -> hashProps n props) (V.head allEntries)
  ]

data Env = Env {
  allEntries  :: !(V.Vector (ByteString, [(ByteString, ByteString)]))
, allStates   :: !(U.Vector Int)
, randEntries :: !(V.Vector (ByteString, [(ByteString, ByteString)]))
, randStates  :: !(U.Vector Int)
}
  deriving stock Generic
  deriving anyclass NFData

setupEnv :: IO Env
setupEnv = do
  let g = mkStdGen 1130
      highestVal = fromIntegral $ natVal (Proxy @HighestBlockStateId)
      xs = take highestVal $ randomRs (0,highestVal) g
  let allEntries = V.generate highestVal idToProps
      allStates  = U.generate highestVal id
      randStates = U.fromList xs
      randEntries = V.generate highestVal (\i -> idToProps (randStates U.! i))
  pure Env{..}
