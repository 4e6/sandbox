import Data.Maybe
import Algs
import Utils

divM :: Integral a => a -> a -> Maybe a
divM _ 0         = Nothing
divM x y
  | mod x y == 0 = Just y
  | otherwise    = Nothing

divDigits :: Integral a => a -> [a]
divDigits x = catMaybes $ map (divM x) (digits x)

main :: IO ()
main = do
  xs <- readInput
  let ss = map (length . divDigits) xs
  sequence_ $ map print ss
