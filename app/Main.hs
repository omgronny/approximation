module Main (main) where

import Cli (parseInput)
import Runner (interpolate)

main :: IO ()
main = do
  (step, window, method) <- parseInput
  interpolate window step method
