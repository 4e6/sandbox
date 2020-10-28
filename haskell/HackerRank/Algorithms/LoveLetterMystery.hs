-- Algorithms/Strings/The Love-Letter Mystery
{-# options_ghc -fno-warn-incomplete-patterns #-}
module HackerRank.Algorithms.LoveLetterMystery where

import Sandbox.Util
import Sandbox.Util.IO

predLowerChar :: Char -> Char
predLowerChar 'a' = 'a'
predLowerChar c = pred c

mkPalindrome :: String -> (Int, String)
mkPalindrome s =
  let (l, r) = splitHalfEq s
      mm = mid s
      (x, a, b) = mkPalindromeBin predLowerChar l (reverse r) 0 [] []
  in case mm of
    Just m  -> (x, a ++ [m] ++ reverse b)
    Nothing -> (x, a ++ reverse b)

-- Warn: pattern matches are non-exhaustive
mkPalindromeBin :: (Ord a) => (a -> a) -> [a] -> [a] -> Int -> [a] -> [a] -> (Int, [a], [a])
mkPalindromeBin _ [] _ n as bs = (n, reverse as, reverse bs)
mkPalindromeBin _ _ [] n as bs = (n, reverse as, reverse bs)
mkPalindromeBin f (a:as) (b:bs) n aps bps
  | a == b = mkPalindromeBin f as bs n (a:aps) (b:bps)
  | a <  b = let c = f b in mkPalindromeBin f (a:as) (c:bs) (n+1) aps bps
  | a >  b = let c = f a in mkPalindromeBin f (c:as) (b:bs) (n+1) aps bps

main :: IO ()
main = do
  ls <- readStringInput
  let ps = map (fst . mkPalindrome) ls
  sequence_ $ map print ps
