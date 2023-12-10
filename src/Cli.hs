{-# LANGUAGE DeriveDataTypeable #-}

module Cli(readPoint, writePoints, parseInput, ProgramInput) where

import qualified Data.Text as T
import Control.Exception
import Text.Printf
import System.Console.CmdArgs

--------------------------------------------------------------------------------

data ProgramInput = ProgramInput {
    step :: Double,
    window :: Int,
    method :: [String]} deriving (Show, Data, Typeable)

pInput :: ProgramInput
pInput = ProgramInput {
    step = def &= help "Iteration frequency",
    window = def &= help "Number of points in window",
    method = def &= help "Interpolation method"
}

parseInput :: IO (Double, Int, [String])
parseInput = do
    input <- cmdArgs pInput
    let (ProgramInput step window method) = input
    return (step, window, method)

--------------------------------------------------------------------------------

readPoint :: IO (Maybe(Double, Double))
readPoint = catch doReadPoint handler

doReadPoint :: IO (Maybe(Double, Double))
doReadPoint = do
    line <- getLine

    let doubles = map T.unpack (T.splitOn (T.pack ":") (T.pack line))
    return (Just (read (doubles!!0) :: Double, read (doubles!!1) :: Double))

handler :: IOError -> IO (Maybe(Double, Double))
handler _ = return Nothing

--------------------------------------------------------------------------------

writePoints :: String -> [Double] -> [Double] -> IO()
writePoints method a b
    | null a = putStr ""
    | otherwise = do
        putStr method
        putStr ": x: "
        printf "%.3f" (head a)
        putStr "; y: "
        printf "%.3f" (head b)
        putStr "\n"
        writePoints method (tail a) (tail b)
