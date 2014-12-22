readCases :: (Integral a, Read a) => IO a
readCases = readLn

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
  n <- readCases
  let r = n + 1
  print r
