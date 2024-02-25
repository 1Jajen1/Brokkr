{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
-- {-# OPTIONS_GHC -ddump-cmm #-}
module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Data.Int
import Data.Kind
import Data.Proxy

import Foreign.Storable

import GHC.Generics

import Hecs

import System.Mem

data Position = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Float {-# UNPACK #-} !Int
  deriving stock (Show, Generic)
  deriving (Storable, Component) via (GenericFlat Position)

data Test

newtype Boxed = Boxed1 Int
  deriving Component via (ViaBox Boxed)
data Color = Red | Green | Blue
  deriving stock (Eq, Show)
  deriving anyclass Component

deriving anyclass instance Component Red
deriving anyclass instance Component Green
deriving anyclass instance Component Blue

makeWorld "World" [
    ''Int
  -- , ''Int8
  , ''Position
  -- , ''Test
  , ''Boxed
  , ''Color, 'Red, 'Green, 'Blue
  ]

main :: IO ()
main = do
  w <- newWorld
  void . runHecsM w $ do
    -- eid <- newEntity
    -- set @Int eid 100
    -- add @(Rel Color Red) eid
    -- add @(Rel Color Green) eid

    -- e2 <- newEntity
    -- add @(Rel Color Blue) e2

    -- q <- query (filterDSL @'[Rel Color Wildcard])
    -- runQuery_ q $ cmapM_ (\(relId :: ComponentId (Rel Color Wildcard), eid :: EntityId) -> do
    --   let (_,snd) = unwrapRelation relId
    --   liftIO $ putStrLn $ "Match: " <> show eid <> " with rel id " <> show snd
    --   )

    -- runFilter_ (filterDSL @'[Rel Color Wildcard]) $ cmapM_ (\(relId :: ComponentId (Rel Color Wildcard), eid :: EntityId) -> do
    --   let (_,snd) = unwrapRelation relId
    --   liftIO $ putStrLn $ "Match: " <> show eid <> " with rel id " <> show snd
    --   )

    system (Proxy @()) (Proxy @()) (filterDSL @'[Rel Color Red]) (\_ _ -> liftIO $ putStrLn "Match") (pure ()) (\_ -> pure ()) -- $ cmapM_ (\() -> do
      -- let (_,snd) = unwrapRelation relId
      -- liftIO $ putStrLn $ "Match: " <> show eid <> " with rel id " <> show snd
    -- e3 <- newEntity
    -- add @(Rel Color Red) e3
    progress 1

  pure ()
