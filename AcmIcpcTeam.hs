import Data.Bits
import Control.Monad
import Utils

data IInteger = IInteger {ind :: Int, int :: Integer}
              deriving (Eq, Show)

instance Ord IInteger where
  compare x y = compare (ind x) (ind y)

fromInteger' :: Integer -> IInteger
fromInteger' i = IInteger (0) i

fromIntegerIor :: Integer -> IInteger
fromIntegerIor i = IInteger (popCount i) i

ior :: IInteger -> IInteger -> IInteger
ior x y = let o = (int x) .|. (int y)
          in fromIntegerIor o

readBinaryString :: IO IInteger
readBinaryString = fmap binToIInt getLine

binToIInt :: String -> IInteger
binToIInt [] = IInteger 0 0
binToIInt bs = let ind  = zip [0..] $ reverse bs
                   pop = map fst $ filter (\p -> snd p == '1') ind
               in fromInteger' $ foldr (flip setBit) 0 pop

combinations :: (a -> a -> a) -> [a] -> [a]
combinations _ []     = []
combinations f (x:xs) = map (f x) xs ++ combinations f xs

bitCombinations :: [IInteger] -> [IInteger]
bitCombinations = combinations ior

main :: IO ()
main = do
  (n,m) <- readPairInt
  xs    <- replicateM n readBinaryString
  let coms = bitCombinations xs
  let best = maximum coms
  let top = filter (==best) coms
  print $ ind best
  print $ length top
