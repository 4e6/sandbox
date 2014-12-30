import qualified Data.List as L
import Util.IO (readSeq)

main :: IO ()
main = do
  x  <- readLn
  _  <- readLn :: IO Int
  xs <- readSeq :: IO [Int]
  let (Just ind) = L.findIndex (==x) xs
  print ind
