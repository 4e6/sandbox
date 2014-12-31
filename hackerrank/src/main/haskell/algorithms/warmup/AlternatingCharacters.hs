import Sandbox.Util.IO

removedDups :: Eq a => [a] -> Int
removedDups as = removed 0 as where
  removed n []  = n
  removed n [_] = n
  removed n (a:b:xs)
    | a == b    = removed (n+1) (a:xs)
    | otherwise = removed n (b:xs)

main :: IO ()
main = do
  ls <- readStringInput
  let rs = map removedDups ls
  sequence_ $ map print rs
