module Main where

import Control.Monad
import Data.Foldable (foldr')
import qualified Sandbox.Util.IO as U

type Op = (Int, Int, Int)

readOps :: Int -> IO [Op]
readOps n = replicateM n $ fmap parse U.readSeq
  where parse (a:b:k:_) = (a,b,k)

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
  print $ truncate avg'
