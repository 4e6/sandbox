module Main (main) where

import qualified Control.Monad   as M
import qualified Data.Maybe      as Mb
import qualified Sandbox.Util.IO as U

type Vertex = Int
type Edge   = (Vertex,Vertex)

data Tree = Node {vertex :: Vertex, edges :: [Tree]} deriving (Eq, Show)

single :: Vertex -> Tree
single x = Node x []

isEven :: Tree -> Bool
isEven = even . size

size :: Tree -> Int
size t = 1 + sum (map size (edges t))

find :: Vertex -> Tree -> Maybe Tree
find x t@(Node v ss)
  | x == v    = Just t
  | otherwise = Mb.listToMaybe $ Mb.mapMaybe (find x) ss

insert :: Edge -> Tree -> Tree
insert e@(from,to) (Node v es)
  | to == v   = Node v $ single from : es
  | otherwise = Node v $ map (insert e) es

insertAll :: [Edge] -> Tree -> Tree
insertAll xs t = foldl (flip insert) t xs

mkTree :: [Edge] -> Tree
mkTree es = insertAll es $ single 1

removeVertex :: Vertex -> Tree -> Tree
removeVertex x (Node v es) = let p = (==x) . vertex
                             in if any p es
                                then Node v $ filter (not . p) es
                                else Node v $ map (removeVertex x) es

remove :: Tree -> Tree -> Tree
remove = removeVertex . vertex

splitTree :: Vertex -> Tree -> (Tree, Tree)
splitTree x t = let Just t' = find x t
                in (t', removeVertex x t)

splitEven' :: Tree -> [Tree] -> Int -> ([Tree], Int)
splitEven' t [] n = ([t], n)
splitEven' t (e:es) n = let (l,r) = splitTree (vertex e) t
                            merge (ts1,n1) (ts2,n2) = (ts1 ++ ts2, n1 + n2)
                        in if isEven l && isEven r
                           then splitEven' l (edges l) n `merge` splitEven' r (edges r) 1
                           else splitEven' t (es ++ edges e) n

splitEven ::  Tree -> ([Tree], Int)
splitEven t@(Node _ es) = splitEven' t es 0

-- n - number of vertices
-- m - number of edges
main :: IO ()
main = do
  (n,m) <- U.readPair
  es    <- M.replicateM m U.readPair
  let (_, x) = splitEven $ mkTree es
  print x
