import Control.Monad

readCases :: (Integral a, Read a) => IO a
readCases = readLn

readInput :: Int -> IO [Int]
readInput n = replicateM n readLn

readInput_ :: (Integral a, Read a) => a -> IO [a]
readInput_ n = sequence $ map (\_ -> readCases) [1..n]

growCycle :: Integral a => a -> a -> a
growCycle c h
  | odd  c = h * 2
  | even c = h + 1

grow :: Integral a => a -> a -> a
grow 0 h = h
grow c h = grown c 1 h where
  grown 0 _ h = h
  grown c n h = grown (c - 1) (n + 1) (growCycle n h)

main :: IO ()
main = do
  n  <- readLn
  cs <- readInput n
  let hs = map (\c -> grow c 1) cs
  sequence_ $ map print hs
