import Control.Monad
import qualified Util.IO as U

readCase' :: IO [Int]
readCase' = getLine >> U.readSeq

readCases :: Int -> IO [[Int]]
readCases n = replicateM n readCase'

main :: IO ()
main = do
  t  <- readLn
  xs <- readCases t
  let cs = map (flip mod 1234567 . product) xs
  sequence_ $ map print cs
