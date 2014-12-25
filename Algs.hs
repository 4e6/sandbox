module Algs where

fibs :: Integral a => [a]
fibs = fib_rec 0 1 where
  fib_rec a b = a : (fib_rec b $ a + b)

fibn :: Integral a => a -> a
fibn 0 = 0
fibn 1 = 1
fibn n = (fibn $ n - 1) + (fibn $ n - 2)

isFib :: Integral a => [a] -> a -> Bool
isFib (f:fs) n =
  if n > f then isFib fs n else n == f
