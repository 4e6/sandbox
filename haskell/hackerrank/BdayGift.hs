-- Mathematics/Probability/B'day Gift

module Main where

import Text.Printf (printf)
import Sandbox.Util.IO (readInput)

main :: IO ()
main = do
  xs <- readInput :: IO [Integer]
  let e = fromIntegral (sum xs) / 2
  printf "%.1f\n" (e :: Double)
