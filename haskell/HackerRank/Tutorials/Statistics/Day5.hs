-- Tutorials/10 Days of Statistics/Day 5
{-# language TypeApplications #-}
module HackerRank.Tutorials.Statistics.Day5 where

import qualified Text.Printf as P
import qualified Sandbox.Statistics as S
import qualified Sandbox.Util.IO as U

-- | Poisson Distribution 1
main1 :: IO ()
main1 = do
  l <- U.readLine @Double
  k <- U.readLine @Int
  let
    res = S.poisson k l
  P.printf "%.3f\n"res
