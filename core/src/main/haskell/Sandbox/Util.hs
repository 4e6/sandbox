module Sandbox.Util where

import Data.List (subsequences)

fibs :: Integral a => [a]
fibs = fib_rec 0 1 where
  fib_rec a b = a : fib_rec b (a+b)

fibn :: Integral a => a -> a
fibn 0 = 0
fibn 1 = 1
fibn n = fibn (n-1) + fibn (n-2)

isFib :: Integral a => [a] -> a -> Bool
isFib (f:fs) n =
  if n > f then isFib fs n else n == f

digits :: Integral a => a -> [a]
digits 0 = []
digits x = let (q, r) = quotRem x 10
           in r : digits q

-- dual to 'digits'
integral :: Integral a => [a] -> a
integral xs = integral_ xs 0 0

integral_ :: Integral a => [a] -> Int -> a -> a
integral_ []     _ s = s
integral_ (x:xs) i s = integral_ xs (i+1) (s + x*10^i)

mid :: [a] -> Maybe a
mid [] = Nothing
mid xs = if even $ length xs then Nothing else Just $ xs !! half (length xs)
  where half x = quot x 2

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt (half $ length xs) xs
  where half x = quot x 2

splitHalfEq :: [a] -> ([a],[a])
splitHalfEq s =
  let (l, r) = splitHalf s
      size = length s
  in if even size then (l, r) else (l, tail r)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = let (l, r) = splitHalfEq xs
                  in l == reverse r

-- obscenely inefficient
sliding :: Int -> [a] -> [[a]]
sliding _ [] = []
sliding n bs@(_:xs) = take n bs : sliding n xs

sliding_ :: Int -> Int -> [a] -> [[a]]
sliding_ _ _ [] = []
sliding_ 0 _ _  = []
sliding_ i n bs@(x:xs) = [x,bs !! (n-1)] : sliding_ (i-1) n xs

fixedWindow :: Int -> [a] -> [[a]]
fixedWindow _ [] = []
fixedWindow n xs = let (l,r) = splitAt n xs
                   in l : fixedWindow n r

combinations :: (a -> a -> a) -> [a] -> [a]
combinations _ []     = []
combinations f (x:xs) = map (f x) xs ++ combinations f xs
