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

readPairInt :: IO (Int, Int)
readPairInt = fmap (parse . words) getLine
  where parse (a:b:_) = (read a, read b)
