module Utils where

import Control.Monad

readLines :: Int -> IO [Int]
readLines n = replicateM n readLn

readInput :: IO [Int]
readInput = readLn >>= readLines
