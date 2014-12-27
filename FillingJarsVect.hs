import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Utils as U (readSeq, readPair)

type Op = (Int, Int, Int)

type Jar s = UM.STVector s Int

readOps :: Int -> IO [Op]
readOps n = replicateM n $ fmap parse U.readSeq
  where parse (a:b:k:_) = (a,b,k)

runOps :: [Op] -> Jar s -> ST s ()
runOps xs jars = forM_ xs $ runOp jars

runOp :: Jar s -> Op -> ST s ()
runOp jars (a,b,v) = let ids = [a..b]
                     in do xs <- forM ids $ UM.read jars
                           let vs = map (+v) xs
                           let zp = zip ids vs
                           forM_ zp $ uncurry $ UM.write jars

average :: Int -> Jar s -> ST s Double
average n jars = do
  xs <- forM [1..n] $ UM.read jars
  let s' = sum xs
      a' = (fromIntegral s') / (fromIntegral n)
  return a'

main :: IO ()
main = do
  (n,m) <- U.readPair
  ops   <- readOps m
  let avg = runST $ do
        jars <- UM.new (n+1)
        UM.set jars 0
        runOps ops jars
        average n jars
  print $ truncate avg
