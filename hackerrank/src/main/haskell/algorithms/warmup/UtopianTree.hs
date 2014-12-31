import Sandbox.Util.IO

growCycle :: Integral a => a -> a -> a
growCycle c h
  | odd  c = h * 2
  | even c = h + 1

grow :: Integral a => a -> a -> a
grow c h = grown c 1 h where
  grown 0 _ h = h
  grown c n h = grown (c - 1) (n + 1) (growCycle n h)

main :: IO ()
main = do
  cs <- readInput
  let hs = map (\c -> grow c 1) cs
  sequence_ $ map print hs
