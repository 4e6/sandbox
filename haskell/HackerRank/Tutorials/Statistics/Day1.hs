-- Tutorials/10 Days of Statistics/Day 1: Standard Deviation
{-# language TypeApplications #-}
module HackerRank.Tutorials.Statistics.Day1 where

import qualified Data.Vector.Unboxed as V
import qualified Text.Printf as P
import qualified Sandbox.Data.Vector.Unboxed.Extra as EV
import qualified Sandbox.Util.IO as U

-- | Calculate first, second and third quartiles
quartiles :: (Fractional b, V.Unbox a, Real a) => V.Vector a -> (b, b, b)
quartiles xs
  | even (V.length xs) =
    let (xl, xr) = V.splitAt middle sorted
    in (EV.median' xl, EV.median' sorted, EV.median' xr)
  | otherwise =
    let
      (xl, right) = V.splitAt middle sorted
      -- exclude middle
      xr = V.drop 1 right
    in (EV.median' xl, realToFrac (sorted V.! middle), EV.median' xr)
  where
    sorted = EV.quicksort xs
    middle = truncate @Double (fromIntegral (V.length xs) / 2)

main :: IO ()
main = do
  _ <- getLine
  xs <- V.fromList <$> U.readSeq @Int
  P.printf "%.1f\n"(EV.std @Double xs)

mainQuartiles :: IO ()
mainQuartiles = do
  _ <- getLine
  xs <- V.fromList <$> U.readSeq @Int
  let (q1, q2, q3) = quartiles @Double xs
  P.printf "%.0f\n%.0f\n%.0f\n" q1 q2 q3

mainInterquartileRange :: IO ()
mainInterquartileRange = do
  _ <- getLine
  xs <- V.fromList <$> U.readSeq @Int
  ws <- V.fromList <$> U.readSeq @Int
  let
    zs = V.concatMap (\(x, w) -> V.replicate w x) (V.zip xs ws)
    (q1, _, q3) = quartiles @Double zs
  P.printf "%.1f\n" (q3 - q1)
