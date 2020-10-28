-- Mathematics/Fundamentals/Filling Jars

module HackerRank.Mathematics.FillingJarsEz where

import Control.Monad
import Data.Foldable (foldr')
import qualified Sandbox.Util.IO as U

type Op = (Int, Int, Int)

readOps :: Int -> IO [Op]
readOps n = replicateM n U.readPair3

add :: Op -> Int -> Int
add (a,b,k) acc = k * (b-a+1) + acc

average :: Int -> Int -> Double
average x y = fromIntegral x / fromIntegral y

main :: IO ()
main = do
  (n,m) <- U.readPair
  ops   <- readOps m
  let sum' = foldr' add 0 ops
      avg' = average sum' n
      res = truncate avg' :: Int
  print res
