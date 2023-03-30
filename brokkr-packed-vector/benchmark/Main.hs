{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Test.Tasty.Bench
import Brokkr.PackedVector
import GHC.Generics
import Control.DeepSeq
import qualified Brokkr.PackedVector.Internal as P
import qualified Brokkr.PackedVector.Mutable.Internal as MP

data Env = Env {
    staticV4  :: PackedVector ('Static 4096) ('Static 4) Int
  , staticV5  :: PackedVector ('Static 4096) ('Static 5) Int
  , staticV6  :: PackedVector ('Static 4096) ('Static 6) Int
  , staticV7  :: PackedVector ('Static 4096) ('Static 7) Int
  , staticV8  :: PackedVector ('Static 4096) ('Static 8) Int
  , staticV9  :: PackedVector ('Static 4096) ('Static 9) Int
  , staticV15 :: PackedVector ('Static 4096) ('Static 15) Int
  }
  deriving stock Generic
  deriving anyclass NFData

setupEnv :: IO Env
setupEnv = do
  
  staticV4  <- MP.new @('Static 4096) @('Static 4)  >>= unsafeFreeze
  staticV5  <- MP.new @('Static 4096) @('Static 5)  >>= unsafeFreeze
  staticV6  <- MP.new @('Static 4096) @('Static 6)  >>= unsafeFreeze
  staticV7  <- MP.new @('Static 4096) @('Static 7)  >>= unsafeFreeze
  staticV8  <- MP.new @('Static 4096) @('Static 8)  >>= unsafeFreeze
  staticV9  <- MP.new @('Static 4096) @('Static 9)  >>= unsafeFreeze
  staticV15 <- MP.new @('Static 4096) @('Static 15) >>= unsafeFreeze

  return $ Env{..}

main :: IO ()
main = do
  defaultMain [
    bgroup "PackedVector"
      [ bgroup "unsafeCopy" $
          [ env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 5 - 4" $ nfIO $ copyArr staticV4 staticV5
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 7 - 4" $ nfIO $ copyArr staticV4 staticV7
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 9 - 4" $ nfIO $ copyArr staticV4 staticV9
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 15 - 4" $ nfIO $ copyArr staticV4 staticV15
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 4 - 5" $ nfIO $ copyArr staticV5 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 4 - 7" $ nfIO $ copyArr staticV7 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 4 - 9" $ nfIO $ copyArr staticV9 staticV4
          , env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 4 - 15" $ nfIO $ copyArr staticV15 staticV4
          ,  env setupEnv $ \ ~(Env{..}) ->
            bench "unsafeCopy 4 - 4" $ nfIO $ copyArr staticV4 staticV4
          ]
      ]
    ]

copyArr :: (P.PVector v a, P.PVector w a) => v a -> w a -> IO ()
copyArr v w = do
  mv <- unsafeThaw v
  mw <- unsafeThaw w
  MP.unsafeCopy mv mw
{-# INLINE copyArr #-}
