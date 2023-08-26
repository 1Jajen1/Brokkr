{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Main (main) where

import Brokkr.HashTable qualified as HT
import Control.Monad.ST (RealWorld)
import Foreign.Storable

type HashTable s k v = HT.HashTable' HT.Storable HT.Boxed s k v

main = pure ()

newtype IntIdHash = IntIdHash Int
  deriving newtype (Eq, Storable)

instance HT.Hash IntIdHash where
  hash (IntIdHash x) = HT.HashFn (const x)

foo :: IO (HashTable RealWorld IntIdHash String)
foo = do
  t <- HT.new 32 initialSalt maxLoadFactor
  HT.insert t (IntIdHash 1) "Hello"
  HT.lookup t (IntIdHash 1)
  HT.delete t (IntIdHash 1)
  pure t
  where
    maxLoadFactor = 0.75
    initialSalt = 0 -- Ideally get this from some random source! 
