{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Main where

import Data.Int

import Control.Monad.IO.Class

import Hecs

import Data.Kind
import GHC.Generics
import Foreign.Storable
import Control.Monad

import Control.Concurrent

data Position = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Float {-# UNPACK #-} !Int
  deriving stock (Show, Generic)
  deriving (Storable, Component) via (GenericFlat Position)

data Test

newtype Boxed = Boxed1 Int
  deriving Component via (ViaBox Boxed)

data Color = Red | Green | Blue
  deriving stock (Eq, Show)
  deriving Component via (ViaBox Color)

makeWorld "World" [
    ''Int
  , ''Int8
  , ''Position
  , ''Test
  , ''Boxed
  , ''Color, 'Red, 'Green, 'Blue
  ]

main :: IO ()
main = do
  w <- newWorld
  void . runHecsM w $ do
    eid <- newEntity
    set @Int eid 100
    set @(Rel Color (Tag Red)) eid $ Rel Red
    set eid $ Pos 30 0.5 2
    eid2 <- newEntity
    set @Int eid2 100
    set eid2 $ Pos 30 0.5 2
    -- defer $ do
    --   eid <- newEntity
    --   set @Boxed eid $ Boxed1 10
    --   set @Int eid 10
    --   get @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    -- liftIO $ print 1
    -- get @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    -- e2 <- newEntity
    -- addTag @Test e2
    -- set e2 (Rel @Color @(Tag Red) Red)
    -- liftIO $ putStrLn "Set tag!"
    -- get @(Rel Color (Tag Red)) e2 (pure . Just) (pure Nothing) >>= liftIO . print
    -- set @Int e2 100
    -- set e2 (Pos 10 20 0)
    -- addTag @Red e2
    -- hasTag @Red e2 >>= liftIO . print
    -- removeTag @Red e2
    -- void . replicateM 65537 $ do
    --   car <- newEntity
    --   liftIO $ print car
    --   set @Int car 100
    --   get @Int car (liftIO . print) (pure ())
    --   addTag @Red car
    --   set @Position car (Pos 10 0 50)
    --   get @Position car (liftIO . print) (pure ())
    --   freeEntity car
    liftIO $ putStrLn "Filter"
    Hecs.filter (filterDSL @'[Int, Rel Color WildCard, Or Boxed Position])
      (\aty _ -> do
        x <- getColumn @Int aty
        es <- getEntityColumn aty
        iterateArchetype aty $ \n e -> do
          liftIO $ print e
          readColumn x n >>= liftIO . print 
        pure ()
        )
      (pure ())
    -- get @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    -- get @Int e2 (pure . Just) (pure Nothing) >>= liftIO . print
    -- hasTag @Red e2 >>= liftIO . print
  putStrLn "Done"
