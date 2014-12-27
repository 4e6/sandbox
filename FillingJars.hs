import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import qualified Utils as U (readSeq, readPair)

type Op = (Int, Int, Int)

type Jar s = STArray s Int Int

readOps :: Int -> IO [Op]
readOps n = replicateM n $ fmap parse U.readSeq
  where parse (a:b:k:_) = (a,b,k)

runOps :: [Op] -> Jar s -> ST s ()
runOps xs jars = forM_ xs $ flip runOp jars

runOp :: Op -> Jar s -> ST s ()
runOp (a,b,v) jars = let ids = [a..b]
                     in do xs <- forM ids (readArray jars)
                           let vs = map (+v) xs
                           forM_ (zip ids vs) (uncurry $ writeArray jars)

average :: Int -> Jar s -> ST s Double
average n jars = do
  es <- getElems jars
  return $ (fromIntegral $ sum es) / (fromIntegral n)

main :: IO ()
main = do
  (n,m) <- U.readPair
  ops   <- readOps m
  let avg = runST $ do
        jars <- newArray (1,n) 0
        runOps ops jars
        average n jars
  print $ truncate avg
