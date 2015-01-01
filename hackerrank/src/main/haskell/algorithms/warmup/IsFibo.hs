module Main where

import Sandbox.Util
import Sandbox.Util.IO

data IF = IsFibo | IsNotFibo deriving (Show)

mkIF :: Bool -> IF
mkIF True  = IsFibo
mkIF False = IsNotFibo

main :: IO ()
main = do
  xs <- readInput
  let ifs = map (mkIF . isFib fibs) xs
  sequence_ $ map print ifs
