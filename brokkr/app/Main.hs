module Main (main) where

import Brokkr.Server
import Brokkr.Server.Config

main :: IO ()
main = newWorld >>= \u -> runServerM defaultConfig u $ setupServer (pure ())
