-- Mathematics/Fundamentals/Minimum Draws

module Main where

import Sandbox.Util.IO (readInput)

main :: IO ()
main = do
  ps <- readInput
  let rs = map (+1) ps
  sequence_ $ map print rs
