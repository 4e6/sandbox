-- Mathematics/Fundamentals/Filling Jars

module HackerRank.Mathematics.FillingJars
  ( main
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import qualified Sandbox.Util.IO as U

type Op = (Int, Int, Int)

type Jar s = STUArray s Int Int

readOps :: Int -> IO [Op]
readOps n = replicateM n U.readPair3

runOps :: [Op] -> Jar s -> ST s ()
runOps xs jars = forM_ xs $ runOp jars

runOp :: Jar s -> Op -> ST s ()
runOp jars (a,b,v) = let ids = [a..b]
                     in do xs <- forM ids $ readArray jars
                           let vs = map (+v) xs
                           let zp = zip ids vs
                           forM_ zp $ uncurry $ writeArray jars

average :: Int -> Jar s -> ST s Double
average n jars = do
  es <- getElems jars
  let s' = sum es
      a' = (fromIntegral s') / (fromIntegral n)
  return a'

main :: IO ()
main = do
  (n,m) <- U.readPair
  ops   <- readOps m
  let avg = runST $ do
        jars <- newArray (1,n) 0
        runOps ops jars
        average n jars
      res = truncate avg :: Int
  print res
