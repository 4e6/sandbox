import Data.Bits
import Control.Monad
import Utils

fixedWindow :: Int -> [a] -> [[a]]
fixedWindow _ [] = []
fixedWindow n xs = let (l,r) = splitAt n xs
                   in l : fixedWindow n r

combinations :: (a -> a -> a) -> [a] -> [a]
combinations _ []     = []
combinations f (x:xs) = map (f x) xs ++ combinations f xs

type Bit = (Int, Bool)

mkBit :: Int -> Char -> Bit
mkBit i '0' = (i, False)
mkBit i '1' = (i, True)
mkBit _ c   = error $ show c ++ " is not a valid binary Char"

mkBits :: String -> [Bit]
mkBits = map (uncurry mkBit) . zip [0..]

significantBits :: [Bit] -> [Int]
significantBits = map fst . filter snd

data IInt = IInt {bits :: [Int]} deriving (Eq, Show)

instance Ord IInt where
  compare x y = compare (pop x) (pop y)

pop :: IInt -> Int
pop = sum . map popCount . bits

-- from bit sequence
mkIInt2 :: [[Bit]] -> IInt
mkIInt2 = mkIInt1 . map significantBits

-- from indices
mkIInt1 :: [[Int]] -> IInt
mkIInt1 = mkIInt . map (foldl setBit 0)

-- from bits
mkIInt :: [Int] -> IInt
mkIInt xs = IInt xs

bitOr :: IInt -> IInt -> IInt
bitOr x y = mkIInt $ zipWith (.|.) (bits x) (bits y)

binToIInt :: String -> IInt
binToIInt xs = let intSize = finiteBitSize (0::Int)
                   chunkss = fixedWindow intSize $ reverse xs
                   bitss   = map mkBits chunkss
               in mkIInt2 bitss

bitCombinations :: [IInt] -> [IInt]
bitCombinations = combinations bitOr

readBinaryString :: IO IInt
readBinaryString = fmap binToIInt getLine

main :: IO ()
main = do
  (n,m) <- readPairInt
  xs    <- replicateM n readBinaryString
  let coms = bitCombinations xs
  let best = maximum coms
  let top = filter (==best) coms
  print $ pop best
  print $ length top
