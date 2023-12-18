module Main (main) where

import Cli (parseInput)
import Runner (interpolate)

main :: IO ()
main = do
  (step, window, methods) <- parseInput
  interpolate window step methods
