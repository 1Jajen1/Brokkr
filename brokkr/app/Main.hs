module Main (main) where

import Server
import Server.Config

main :: IO ()
main = newWorld >>= \u -> runServerM defaultConfig u $ setupServer (pure ())
