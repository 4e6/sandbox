module Sandbox.Util.IO where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (mapMaybe)

readLines :: Read a => Int -> IO [a]
readLines n = replicateM n readLn

getLines :: Int -> IO [String]
getLines n = replicateM n getLine

readInput :: Read a => IO [a]
readInput = readLn >>= readLines

readInput' :: Read a => IO a -> IO [a]
readInput' f = readLn >>= flip replicateM f

readStringInput :: IO [String]
readStringInput = readLn >>= getLines

readSeq :: Read a => IO [a]
readSeq = fmap (parse . words) getLine
  where parse = map read

readPair :: Read a => IO (a, a)
readPair = fmap parse readSeq
  where parse (a:b:_) = (a, b)

readPair3 :: Read a => IO (a,a,a)
readPair3 = fmap parse readSeq
  where parse (a:b:c:_) = (a,b,c)

readPair4 :: Read a => IO (a,a,a,a)
readPair4 = fmap parse readSeq
  where parse (a:b:c:d:_) = (a,b,c,d)

readSeqInt :: IO [Int]
readSeqInt = fmap readInts' getLine where
  readInts' = map fst . mapMaybe BS.readInt . BS.words . BS.pack
