module Main (main) where

import Spec (spec)

import Test.Syd

import Utils.Setup

main :: IO ()
main = sydTest $ withBrokkrServerSpec spec
