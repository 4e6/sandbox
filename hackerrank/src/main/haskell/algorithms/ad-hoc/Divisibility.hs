module Main (main) where

import qualified Control.Monad as M
import Sandbox.Util (digits, integral)
import Sandbox.Util.IO (readPair)

subsequence' :: [a] -> [a] -> [[a]]
subsequence' []     _ = []
subsequence' (x:xs) bs = (x:bs) : subsequence' xs (x:bs)

contSubsequences :: [a] -> [[a]]
contSubsequences [] = []
contSubsequences xs = subsequence' xs [] ++ contSubsequences (tail xs)

-- as reversed
sublist :: Int -> Int -> [a] -> [a]
sublist x y as = let s = length as
                 in take (y-x+1) . drop (s-y) $ as

subints :: Integral a => [a] -> [a]
subints xs = map integral $ contSubsequences xs

divisible :: Integral a => a -> a -> Bool
divisible s p = mod s p == 0

main :: IO ()
main = do
  (p,q) <- readPair
  s     <- readLn
  ints  <- M.replicateM q readPair
  let ds = digits s
      sublist' = flip (uncurry sublist)
      subints' = map (subints . sublist' ds) ints
      divisible' = map (length . filter (`divisible` p)) subints'
  M.forM_ divisible' print
