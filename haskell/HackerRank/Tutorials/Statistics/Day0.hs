-- Tutorials/10 Days of Statistics/Day 0: Mean, Median, and Mode
{-# language TypeApplications #-}
module HackerRank.Tutorials.Statistics.Day0 where

import qualified Data.Vector.Unboxed as V
import qualified Text.Printf as P
import qualified Sandbox.Data.Vector.Unboxed.Extra as EV
import qualified Sandbox.Util.IO as U

main :: IO ()
main = do
  _ <- getLine
  xs <- V.fromList <$> U.readSeq @Int
  P.printf "%.1f\n"(EV.mean xs)
  P.printf "%.1f\n" (EV.median @Double xs)
  print (EV.mode xs)

-- Weighted mean
mainWeighted :: IO ()
mainWeighted = do
  _ <- getLine
  xs <- V.fromList <$> U.readSeq @Int
  ws <- V.fromList <$> U.readSeq @Int
  P.printf "%.1f\n" (EV.meanWeighted @Double xs ws)
