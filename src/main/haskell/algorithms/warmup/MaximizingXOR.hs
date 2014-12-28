import Data.Bits

maxXor :: Int -> Int -> Int
maxXor l r = maximum $ map (uncurry xor) cs
  where cs = [(a, b) | a <- [l..r], b <- [a..r]]

main :: IO ()
main = do
  l <- readLn
  r <- readLn
  print $ maxXor l r
