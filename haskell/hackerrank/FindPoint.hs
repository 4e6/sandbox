-- Mathematics/Fundamentals/Find Point

module Main where

import Data.Tuple.Curry (uncurryN)
import Sandbox.Util.IO

symmetryPoint :: Num a => a -> a -> a -> a -> (a, a)
symmetryPoint px py qx qy = let dx = qx - px
                                dy = qy - py
                            in (qx + dx, qy + dy)

printPair :: Show a => (a, a) -> IO ()
printPair (x, y) = putStrLn $ show x ++ " " ++ show y

main :: IO ()
main = do
  ps <- readInput' readPair4
  let rs = map (uncurryN symmetryPoint) ps
  sequence_ $ map printPair rs
