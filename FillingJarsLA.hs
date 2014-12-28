import Control.Monad
import Data.Foldable (foldl')
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix
import qualified Utils as U

type Op = (Int, Int, Int)

readOps :: Int -> IO [Op]
readOps n = replicateM n $ fmap parse U.readSeq
  where parse (a:b:k:_) = (a,b,k)

opVector' :: Int -> Op -> Vector Double
opVector' n (a,b,k) =
  vjoin [konst 0 (a-1),
         konst (fromIntegral k) (b-a+1),
         konst 0 (n-b)]

foldOp :: Int -> Vector Double -> Op -> Vector Double
foldOp n v op = v + opVector' n op

average :: Int -> Vector Double -> Double
average n v = sumElements v / fromIntegral n

main :: IO ()
main = do
  (n,m) <- U.readPair
  ops   <- readOps m
  let filled = foldl' (foldOp n) (vector [0]) ops
      avg    = average n filled
  print $ truncate avg
