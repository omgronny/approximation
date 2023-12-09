module Main (main) where

import Runner(interpolate)

import Cli(parseInput)

-- cabal run approximation-exe --verbose=0

main :: IO()
main =  do
    (step, window, method) <- parseInput
    interpolate window step method
