import Util.IO

countHandshakes :: Int -> Int
countHandshakes x = count' x 0

count' :: Int -> Int -> Int
count' 0 acc = acc
count' 1 acc = acc
count' x acc = count' (x-1) acc+x-1

main :: IO ()
main = do
  xs <- readInput
  let hs = map countHandshakes xs
  sequence_ $ map print hs
