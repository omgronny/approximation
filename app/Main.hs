module Main (main) where

import Cli (parseInput)
import Runner (interpolate)

-- cabal run approximation-exe --verbose=0

main :: IO ()
main = do
  (step, window, method) <- parseInput
  interpolate window step method
