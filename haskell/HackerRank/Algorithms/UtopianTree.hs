-- Algorithms/Implementation/Utopian Tree
{-# language TypeApplications #-}
{-# options_ghc -fno-warn-incomplete-patterns #-}
module HackerRank.Algorithms.UtopianTree where

import Sandbox.Util.IO

-- Warn: Pattern match(es) are non-exhaustive
growCycle :: Integral a => a -> a -> a
growCycle c h
  | odd  c = h * 2
  | even c = h + 1

grow :: Integral a => a -> a -> a
grow c h = grown c 1 h where
  grown 0 _ ih = ih
  grown ic n ih = grown (ic - 1) (n + 1) (growCycle n ih)

main :: IO ()
main = do
  cs <- readInput @Int
  let hs = map (\c -> grow c 1) cs
  sequence_ $ map print hs
