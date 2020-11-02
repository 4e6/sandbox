module Sandbox.Statistics where

import qualified Sandbox.Number.Extra as N

-- | Permutation of r objects in set of n objects.
permutations :: (Fractional b, Integral a) => a -> a -> b
permutations n r = fromIntegral (N.fact n) / fromIntegral (N.fact (n-r))

-- | Combination of r objects in set of n objects.
combinations :: (Fractional b, Integral a) => a -> a -> b
combinations n r = permutations n r / fromIntegral (N.fact r)

-- | Binomial distribution.
--
-- * `x` - the number of succeses.
-- * `n` - the total number of trials.
-- * `p` - the probability of success of 1 trial.
binomial :: (Fractional b, Integral a) => a -> a -> b -> b
binomial x n p =
  combinations n x * (p ^ x) * ((1-p) ^ (n-x))

-- | Negative binomial distribution.
negative_binomial :: (Fractional b, Integral a) => a -> a -> b -> b
negative_binomial x n p =
  combinations (n-1) (x-1) * (p ^ x) * ((1-p) ^ (n-x))

-- | Geometric distribution.
--
-- * `x` - the number of succeses.
-- * `n` - the total number of trials.
-- * `p` - the probability of success of 1 trial.
geometric :: (Fractional b, Integral a) => a -> a -> b -> b
geometric x n p = (1-p) ^ (n-x) * p

-- | Poisson distribution.
--
-- * `k` is the actual number of successes that occur in a specified region.
-- * `l` is the average number of successes that occur in a specified region.
poisson :: (Floating b, Integral a) => a -> b -> b
poisson k l = (l ^ k) * exp (-l) / fromIntegral (N.fact k)
