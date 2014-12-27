import Control.Monad (replicateM)
import qualified Data.HashMap as M
import qualified Utils as U (readSeq, readPair)

type Op = (Int, Int, Int)

type Jars = M.Map Int Integer

readOps :: Int -> IO [Op]
readOps n = replicateM n $ fmap parse U.readSeq
  where parse (a:b:k:_) = (a,b,k)

runOps :: [Op] -> Jars
runOps ops = runOps_ ops M.empty

runOps_ :: [Op] -> Jars -> Jars
runOps_ []     m = m
runOps_ (x:xs) m = runOps_ xs $ runOp x m

runOp :: Op -> Jars -> Jars
runOp (a,b,v) jars = foldl put jars [a..b]
  where put m k = M.insertWith (+) k (toInteger v) m

msum :: Integral a => M.Map k a -> a
msum = M.fold (+) 0

main :: IO ()
main = do
  (n,m) <- U.readPair
  ops   <- readOps m
  let jars = runOps ops
  let avg  = fromIntegral (msum jars) / (fromIntegral n)
  print $ truncate avg
