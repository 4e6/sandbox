module Main where

import Control.Monad
import qualified Sandbox.Util.IO as U

insertionSort :: Ord a => [a] -> [[a]]
insertionSort [] = []
insertionSort xs = insertionSort' (init xs) (last xs) []

insertionSort' :: Ord a => [a] -> a -> [a] -> [[a]]
insertionSort' [] x rs = [x:rs]
insertionSort' ls x rs = let z  = last ls
                             as = init ls
                             ps = ls ++ [z] ++ rs
                         in if x > z
                            then [ls ++ [x] ++ rs]
                            else ps : insertionSort' as x (z:rs)

showList' :: Show a => [a] -> String
showList' = unwords . map show

main :: IO ()
main = do
  xs <- (readLn :: IO Int) >> U.readSeqInt
  let ss = insertionSort xs
  forM_ ss $ putStrLn . showList'
