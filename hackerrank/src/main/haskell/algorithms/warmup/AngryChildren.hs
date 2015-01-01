module Main where

import Control.Monad
import Data.List (sort)
import qualified Data.Sequence as S
import qualified Data.Array as A
import Numeric.LinearAlgebra.Data

sliding :: Int -> Int -> Int -> Vector Double -> [(Double, Double)]
sliding l r n v
  | r == n    = []
  | otherwise = (v ! l, v ! r) : sliding (l+1) (r+1) n v

slidingS :: Int -> Int -> Int -> S.Seq Int -> [(Int, Int)]
slidingS l r n s
  | r == n    = []
  | otherwise = (S.index s l, S.index s r) : slidingS (l+1) (r+1) n s

slidingA :: Int -> Int -> Int -> A.Array Int Int -> [(Int, Int)]
slidingA l r n a
  | r == n    = []
  | otherwise = (a A.! l, a A.! r) : slidingA (l+1) (r+1) n a

distance :: Num a => a -> a -> a
distance x y = y - x

knn :: Int -> Int -> [Double] -> [Int]
knn n k xs = let v = sortVector $ fromList xs
             in map (truncate . uncurry distance) $ sliding 0 (k-1) n v

knnS :: Int -> Int -> [Int] -> [Int]
knnS n k xs = let ss = S.fromList $ sort xs
              in map (uncurry distance) $ slidingS 0 (k-1) n ss

knnA :: Int -> Int -> [Int] -> [Int]
knnA n k xs = let as = A.listArray (0,n-1) $ sort xs
              in map (uncurry distance) $ slidingA 0 (k-1) n as

main :: IO ()
main = do
  n  <- readLn
  k  <- readLn
  xs <- replicateM n readLn
  let ds = knn n k xs
  print $ minimum ds
