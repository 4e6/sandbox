{-# LANGUAGE FlexibleContexts #-}
module Sandbox.Util.IO where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (mapMaybe)

readLine :: Read a => IO a
readLine = read <$> getLine

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

readPair :: (Show a, Read a) => IO (a, a)
readPair = parse =<< readSeq
  where
    parse (a:b:_) = pure (a, b)
    parse a       = fail ("Unexpected input: " ++ show a)

readPair3 :: (Show a, Read a) => IO (a,a,a)
readPair3 = parse =<< readSeq
  where
    parse (a:b:c:_) = pure (a,b,c)
    parse a         = fail ("Unexpected input: " ++ show a)

readPair4 :: (Show a, Read a) => IO (a,a,a,a)
readPair4 = parse =<< readSeq
  where
    parse (a:b:c:d:_) = pure (a,b,c,d)
    parse a           = fail ("Unexpected input: " ++ show a)

readSeqInt :: IO [Int]
readSeqInt = fmap readInts' getLine where
  readInts' = map fst . mapMaybe BS.readInt . BS.words . BS.pack
