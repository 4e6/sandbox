-- Tutorials/10 Days of Statistics/Day 4
{-# language TypeApplications #-}
module HackerRank.Tutorials.Statistics.Day4 where

import qualified Text.Printf as P
import qualified Sandbox.Statistics as S
import qualified Sandbox.Util.IO as U

-- | Binomial Distribution 1
main1 :: IO ()
main1 = do
  (a, b) <- U.readPair @Double
  let
    p = a / (a + b)
    n = 6 :: Int
    -- at least 3
    res = fmap (\x -> S.binomial x n p) [3..n]
  P.printf "%.3f\n"(sum res)

-- | Binomial Distribution 2
main2 :: IO ()
main2 = do
  (p', n) <- U.readPair @Int
  let
    -- p rejected
    p = (fromIntegral p') / 100 :: Double
    -- no more that 2 rejects
    res1 = fmap (\x -> S.binomial x n p) [0..2]
    -- at least 2 rejects
    res2 = fmap (\x -> S.binomial x n p) [2..n]
  P.printf "%.3f\n%.3f\n"(sum res1) (sum res2)

-- | Geometric Distribution 1
main3 :: IO ()
main3 = do
  (a, b) <- U.readPair @Int
  n <- U.readLine @Int
  let
    p = fromIntegral a / fromIntegral b :: Double
    res = S.geometric 1 n p
  P.printf "%.3f\n" res

-- | Geometric Distribution 2
main4 :: IO ()
main4 = do
  (a, b) <- U.readPair @Int
  n <- U.readLine @Int
  let
    p = fromIntegral a / fromIntegral b :: Double
    -- during the first 5 inspections
    res = fmap (\x -> S.geometric x n p) [1..5]
  P.printf "%.3f\n" (sum res)
