import Data.Bits
import Data.Char (intToDigit)
import Data.Word (Word)
import Numeric (showIntAtBase)
import Control.Monad (replicateM)
import Algs (combinations, fixedWindow)
import Utils (readPair)

type Bit = (Int, Bool)

mkBit :: Int -> Char -> Bit
mkBit i '0' = (i, False)
mkBit i '1' = (i, True)
mkBit _ c   = error $ show c ++ " is not a valid binary Char"

mkBits :: String -> [Bit]
mkBits = map (uncurry mkBit) . zip [0..]

significantBits :: [Bit] -> [Int]
significantBits = map fst . filter snd

data IInt = IInt {bits :: [Word]} deriving (Eq, Show)

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
mkIInt :: [Word] -> IInt
mkIInt xs = IInt (reverse xs)

bitOr :: IInt -> IInt -> IInt
bitOr x y = mkIInt $ zipWith (.|.) (bits x) (bits y)

binToIInt :: String -> IInt
binToIInt xs = let intSize = finiteBitSize (0::Word)
                   chunkss = fixedWindow intSize $ reverse xs
                   bitss   = map mkBits chunkss
               in mkIInt2 bitss

bitCombinations :: [IInt] -> [IInt]
bitCombinations = combinations bitOr

readBinaryString :: IO IInt
readBinaryString = fmap binToIInt getLine

showBin :: (Integral a, Show a) => a -> String
showBin x = showIntAtBase 2 intToDigit x ""

showIInt :: IInt -> String
showIInt = concat . map showBin . bits

main :: IO ()
main = do
  (n,m) <- readPair
  xs    <- replicateM n readBinaryString
  let coms = bitCombinations xs
  let best = maximum coms
  let top = filter ((==) EQ . compare best) coms
  print $ pop best
  print $ length top
