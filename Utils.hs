module Utils where

import Control.Monad

readLines :: Read a => Int -> IO [a]
readLines n = replicateM n readLn

getLines :: Int -> IO [String]
getLines n = replicateM n getLine

readInput :: Read a => IO [a]
readInput = readLn >>= readLines

readStringInput :: IO [String]
readStringInput = readLn >>= getLines
