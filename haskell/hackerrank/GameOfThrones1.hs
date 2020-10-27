-- Algorithms/Strings/Game of Thrones - I

module Main where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M

data Answer = YES | NO deriving (Show)

mkAnswer :: Bool -> Answer
mkAnswer True  = YES
mkAnswer False = NO

freqMap :: (Hashable a, Ord a) => [a] -> M.HashMap a Int
freqMap xs = freqMap_ xs M.empty

freqMap_ :: (Hashable a, Ord a) => [a] -> M.HashMap a Int -> M.HashMap a Int
freqMap_ []     m = m
freqMap_ (x:xs) m = freqMap_ xs $ M.insertWith (+) x 1 m

isSymmetric :: (Hashable a, Ord a) => [a] -> Bool
isSymmetric xs = let eles = M.elems $ freqMap xs
                     odds = filter odd eles
                 in length odds <= 1

main :: IO ()
main = do
  s <- getLine
  let a = mkAnswer $ isSymmetric s
  print a
