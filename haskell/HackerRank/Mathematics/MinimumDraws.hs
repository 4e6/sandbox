-- Mathematics/Fundamentals/Minimum Draws
{-# language TypeApplications #-}
module HackerRank.Mathematics.MinimumDraws where

import Sandbox.Util.IO (readInput)

main :: IO ()
main = do
  ps <- readInput @Int
  let rs = map (+1) ps
  sequence_ $ map print rs
