module Main (main) where

import Control.Monad as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import qualified Data.Maybe  as Mb
import Sandbox.Util.IO (readSeqInt, readPair)

readInput :: IO (Int, IntSet)
readInput = do
  (_,k) <- readPair
  xs    <- readSeqInt
  return (k, Set.fromList xs)

mods :: Int -> IntSet -> IntSet
mods k xs = Set.map (mod k) $ Set.filter (<=k) xs

multisum1M :: Int -> IntSet -> Maybe Int
multisum1M k xs = let min' = Set.minView $ mods k xs
                  in fmap ((k-) . fst) min'

multisum1 :: Int -> IntSet -> Int
multisum1 k xs = Mb.fromMaybe 0 $ multisum1M k xs

multirem :: Int -> IntSet -> Int
multirem k ms = k - multisum1 k ms

multisum_ :: Int -> IntSet -> IntSet -> IntSet -> Int
multisum_ k xs ms rs
  | Set.null xs     || Set.null ms     = 0
  | Set.member 0 ms || Set.member 0 rs = k
  | Set.null rs                        = k - Set.findMin ms
  | otherwise                          = k - Set.findMin rs

multisum :: Int -> IntSet -> Int
multisum k xs = let ms = mods k xs
                    rs = Set.map (`multirem` xs) ms
                in multisum_ k xs ms rs

main :: IO ()
main = do
  t  <- readLn
  ps <- M.replicateM t readInput
  let res = map (uncurry multisum) ps
  M.forM_ res print
