module Main (main) where

import qualified Control.Monad as M
import Sandbox.Util.IO (readPair)

slice :: Integral a => Int -> a -> a -> a -> a
slice s x b e = let b' = fromIntegral s - b + 1
                    e' = fromIntegral s - e
                in truncate $ fromIntegral (mod x (10^b')) / fromIntegral (10^e')

intervals :: Integral a => a -> a -> [(a,a)]
intervals b' e' = intervals_ b' e' b' []
  where intervals_ b e i acc
          | b <= e && i <= e = intervals_  b    e (i+1) $ (b,i):acc
          | b <= e && i >  e = intervals_ (b+1) e (b+1) acc
          | otherwise        = acc

ints :: Integral a => Int -> a -> a -> a -> [a]
ints s x b e = let xlice = uncurry $ slice s x
               in map xlice $ intervals b e

divisible :: Integral a => a -> a -> Bool
divisible s p = mod s p == 0

main :: IO ()
main = do
  (p,q) <- readPair
  str   <- getLine
  pairs <- M.replicateM (fromIntegral q) readPair
  let size' = length str
      s'    = read str :: Integer
      ints' = uncurry $ ints size' s'
      divs' = map (length . filter (`divisible` p) . ints') pairs
  M.forM_ divs' print
