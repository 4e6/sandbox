module Sandbox.Number.Extra where

-- | Factorial
fact :: (Eq a, Num a) => a -> a
fact = fact' 1
  where
    fact' s 0 = s
    fact' s a = fact' (s * a) (a - 1)
