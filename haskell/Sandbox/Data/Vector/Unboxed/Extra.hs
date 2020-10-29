{-# language TypeApplications #-}
{-# language ViewPatterns #-}
module Sandbox.Data.Vector.Unboxed.Extra where

import qualified Data.Vector.Unboxed as V
import qualified Data.Map.Strict as M

quicksort :: (V.Unbox a, Ord a) => V.Vector a -> V.Vector a
quicksort xs
  | V.null xs = xs
  | otherwise =
    let
      (hds, tl) = V.splitAt 1 xs
      hd = V.head hds
      smallerSorted = quicksort . V.filter (<= hd) $ tl
      biggerSorted = quicksort . V.filter (> hd) $ tl
    in smallerSorted V.++ (V.singleton hd) V.++ biggerSorted

-- | The average number.
--
-- The sum of the values divided by the number of values.
mean :: (Fractional b, V.Unbox a, Real a) => V.Vector a -> b
mean xs = (realToFrac (V.sum xs)) / (fromIntegral (V.length xs))

-- | Weighted mean
--
-- The weighted sum of the values divided by the number of values.
meanWeighted :: (Fractional b, V.Unbox a, Real a) => V.Vector a -> V.Vector a -> b
meanWeighted xs ws = realToFrac weightedSum / realToFrac weight
  where
    weightedSum = V.foldl' (\m (x, w) -> m + x*w) 0 (V.zip xs ws)
    weight = V.sum ws

-- | The middle number.
--
-- Found by ordering all data points and picking out the one in the middle, or
-- if there are two middle numbers, taking the mean of those two numbers.
median :: (Fractional b, V.Unbox a, Real a) => V.Vector a -> b
median (quicksort -> xs)
  | even (V.length xs) = realToFrac ((xs V.! (middle-1)) + xs V.! middle) / 2
  | otherwise          = realToFrac (xs V.! middle)
    where
      middle = truncate @Double (fromIntegral (V.length xs) / 2)

-- | The value that appears most often.
--
-- If there are several values that appear the same number of times, take the
-- smallest of them.
mode :: (V.Unbox a, Ord a, Num a) => V.Vector a -> a
mode xs = fst $ M.foldlWithKey' makeMode (hd, 0) occurrences
  where
    hd = V.head xs
    occurrences = V.foldl (\m a -> M.insertWith (+) a (1::Int) m) M.empty xs
    makeMode (ma, mi) a i
      | i > mi    = (a, i)
      | i == mi   = (min a ma, i)
      | otherwise = (ma, mi)
