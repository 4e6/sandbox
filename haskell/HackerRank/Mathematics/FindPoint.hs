-- Mathematics/Fundamentals/Find Point
{-# language TypeApplications #-}
module HackerRank.Mathematics.FindPoint where

import qualified Sandbox.Util.IO as U

symmetryPoint :: Num a => a -> a -> a -> a -> (a, a)
symmetryPoint px py qx qy =
  let dx = qx - px
      dy = qy - py
  in (qx + dx, qy + dy)

printPair :: Show a => (a, a) -> IO ()
printPair (x, y) = putStrLn $ show x ++ " " ++ show y

main :: IO ()
main = do
  ps <- U.readInput' (U.readPair4 @Int)
  let rs = map (\(a, b, c, d) -> symmetryPoint a b c d) ps
  sequence_ $ map printPair rs
