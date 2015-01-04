module Main (main) where

import qualified Control.Monad   as M
import qualified Data.Maybe      as Mb
import qualified Sandbox.Util.IO as U

type Vertex = Int
type Edge   = (Vertex,Vertex)

data Tree = Node {vertex :: Vertex, edges :: [Tree]} deriving (Show)

single :: Vertex -> Tree
single x = Node x []

--splice :: Tree -> Tree -> Tree
--splice from (Node v es) = Node v (from:es)

size :: Tree -> Int
size t = 1 + sum (map size (edges t))

mkTree :: [Edge] -> Tree
mkTree es = insertAll es $ single 1

insertAll :: [Edge] -> Tree -> Tree
insertAll xs t = foldl (flip insert) t xs

insert :: Edge -> Tree -> Tree
insert e@(from,to) tree@(Node v es)
  | to == v   = Node v $ single from : es
  | otherwise = Node v $ map (insert e) es

remove :: Tree -> Tree -> Tree
remove xt@(Node x _) (Node v es) = let p = (==x) . vertex
                                   in if any p es
                                      then Node v $ filter (not . p) es
                                      else Node v $ map (remove xt) es

find :: Vertex -> Tree -> Maybe Tree
find x t@(Node v ss)
  | x == v    = Just t
  | otherwise = Mb.listToMaybe $ Mb.mapMaybe (find x) ss

findParent :: Vertex -> [Edge] -> Maybe Vertex
findParent _ []         = Nothing
findParent v ((f,t):es)
  | v == f    = Just t
  | otherwise = findParent v es

isEven :: Tree -> Bool
isEven = even . size

findEven :: Vertex -> [Edge] -> Tree-> Maybe Tree
findEven v es t = case find v t of
                   Nothing -> Nothing
                   Just t' -> if isEven t'
                              then Just t'
                              else case findParent v es of
                                    Nothing -> Nothing
                                    Just p  -> findEven p es t

evenForest :: [Edge] -> Tree -> [Tree] -> [Tree]
evenForest [] t as = as
evenForest es'@((fr,to):es) t as = case findEven fr es' t of
                                    Nothing -> evenForest es t as
                                    Just t' -> evenForest es (remove t' t) (t':as)

splitEven :: [Edge] -> Tree -> [Tree]
splitEven es t = evenForest es t []

-- n - number of vertices
-- m - number of edges
main :: IO ()
main = do
  (n,m) <- U.readPair
  es    <- M.replicateM m U.readPair
  let forest = splitEven (reverse es) (mkTree es)
  putStrLn "tree:"
  print $ mkTree es
  putStrLn "forest:"
  M.forM_ forest print
  print $ length forest
