-- Data Structures/Advanced/Divisibility

module Main (main) where

import Control.Monad.ST
import qualified Control.Monad as M
import qualified Data.Map.Strict as Map
import Data.STRef
import System.IO.Unsafe
import Sandbox.Util.IO (readPair)

memoize :: Ord a => (a -> ST s b) -> ST s (a -> ST s b)
memoize f = do
  mc <- newSTRef Map.empty
  return $ \k -> do
    c <- readSTRef mc
    case Map.lookup k c of
     Just  v -> return v
     Nothing -> do v <- f k
                   writeSTRef mc (Map.insert k v c) >> return v

unsafeMemoize :: Ord a => (a -> b) -> a -> b
unsafeMemoize f = unsafePerformIO $ do
  f' <- stToIO $ memoize $ return . f
  return $ unsafePerformIO . stToIO . f'

slice :: Integral a => Int -> a -> a -> a -> a
slice s x b e = let b' = fromIntegral s - b + 1
                    e' = fromIntegral s - e
                in quot (mod x (10^b')) (10^e')

intervals :: Integral a => a -> a -> [(a,a)]
intervals b' e' = intervals_ b' e' b' []
  where intervals_ b e i acc
          | b <= e && i <= e = intervals_  b    e (i+1) $ (b,i):acc
          | b <= e && i >  e = intervals_ (b+1) e (b+1) acc
          | otherwise        = acc

ints :: Integral a => Int -> a -> a -> a -> [a]
ints s x b e = let xlice = uncurry $ slice s x
               in map xlice $ intervals b e

intsf :: Integral a => ((a,a) -> a) -> a -> a -> [a]
intsf f b e = map f $ intervals b e

divisible :: Integral a => a -> a -> Bool
divisible s p = mod s p == 0

main :: IO ()
main = do
  (p,q) <- readPair
  str   <- getLine
  pairs <- M.replicateM (fromIntegral q) readPair
  let size' = length str
      s'    = read str :: Integer
      memslice = unsafeMemoize $ uncurry $ slice size' s'
      divp = (`divisible` p)
      intsf' = uncurry $ intsf memslice
--      pints' = map intsf' pairs
--      ss = map length pints'
--      divs1 = map (length . filter (`divisible` p)) pints'
      divs' = map (length . filter divp . intsf') pairs
  M.forM_ divs' print
