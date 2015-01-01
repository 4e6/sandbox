module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as S
import Data.Maybe (catMaybes)
import Data.Foldable (foldr')

readSeqInt :: IO [Int]
readSeqInt = fmap readInts' getLine where
  readInts' = map fst . catMaybes . map S.readInt . S.words . S.pack

readInput :: Int -> IO [[Int]]
readInput n = replicateM n $ getLine >> readSeqInt

maxSubseq :: [Int] -> Int
maxSubseq xs = fst $ maxSubseq' xs 0 0 0 (minBound :: Int) 0 0

maxSubseq' :: [Int] ->
              Int -> Int -> Int ->
              Int -> Int -> Int ->
              (Int, (Int, Int))
maxSubseq' [] _ _ _ maxSum maxSrt maxEnd = (maxSum, (maxSrt, maxEnd))
maxSubseq' (x:xs) curSrt curEnd curSum maxSum maxSrt maxEnd =
  let curSum' = curSum + x
      curEnd' = curEnd + 1
      (maxSum', maxSrt', maxEnd') = if curSum' > maxSum
                                    then (curSum', curSrt, curEnd)
                                    else (maxSum, maxSrt, maxEnd)
      (curSum1', curSrt') = if curSum' < 0 then (0, curEnd+1) else (curSum', curSrt)
  in maxSubseq' xs curSrt' curEnd' curSum1' maxSum' maxSrt' maxEnd'

maxNoncontigSubseq :: [Int] -> Int
maxNoncontigSubseq xs
  | all (<0) xs = maximum xs
  | otherwise   = foldr' sumPositive 0 xs
  where sumPositive x s = if x > 0 then x+s else s

showTuple :: Show a => (a,a) -> String
showTuple (x,y) = show x ++ " " ++ show y

main :: IO ()
main = do
  t  <- readLn
  xs <- readInput t
  let ss = map maxSubseq xs
      ns = map maxNoncontigSubseq xs
  forM_ (zip ss ns) $ putStrLn . showTuple
