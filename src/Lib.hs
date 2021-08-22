module Lib where
import Data.List
someFunc :: IO ()
someFunc = putStrLn "someFunc"
type SymList a = ([a], [a])
minWith :: Ord b => (a -> b) -> [ a ] -> a
minWith f = foldr1 (smaller f)
            where smaller f x y = if f x <= f y then x else y

-- sort :: Ord a => [a] -> [a]
-- sort = minWith ic.perms
-- ic :: Ord a => [a] -> Int
pairs :: [a] -> [(a,a)]
pairs xs = [(x, y) | x:ys <- tails xs, 
                     y:zs <- tails ys]

ic :: Ord a => [a] -> Int
ic xs = length [(x,y) | (x,y) <- pairs xs, x > y]

extend :: a -> [a] -> [[a]]
extend x [ ] = [ [ x ] ]
extend x (y:xs) = (x:y:xs):map (y:) (extend x xs)

perms :: [a] -> [[a]]
perms = foldr (concatMap . extend) [ [ ] ]
type Nat = Int
type Denom = Nat
type Tuple = [Nat]
usds, ukds :: [Denom]
usds = [100,50,25,10,5,1]
ukds = [200,100,50,20,10,5,2,1]

amount :: [ Denom ] -> Tuple -> Nat 
amount ds cs = sum (zipWith (*) ds cs)

mktuples :: [ Denom ] -> Nat -> [ Tuple ]
mktuples [1] n = [[n]]
mktuples (d : ds) n = [ c : cs | c <- [ 0 .. n `div` d ], cs <- mktuples ds (n - c * d) ]
mkchange :: [ Denom ] -> Nat -> Tuple
mkchange ds = minWith sum . mktuples ds

data Tree a = Leaf a | Node (Tree a) (Tree a)
size :: Tree a -> Nat
size (Leaf x) = 1
size (Node u v) = size u+size v

height :: Tree a1 -> Int
height (Leaf x) = 0
height (Node u v) = 1 + height u `max` height v

fringe :: Tree a -> [a]
fringe (Leaf x) = [x]
fringe (Node u v) = fringe u ++ fringe v


mktree :: [a] -> Tree a
mktree [x] = Leaf x
mktree xs = Node(mktree ys)(mktree zs)
            where (ys, zs) = splitAt (length xs `div` 2) xs


type Vertex = Int
type Weight = Int
type Edge = (Vertex, Vertex, Weight)
type Graph  = ([Vertex], [Edge])
nodes :: Graph -> [Vertex]
nodes (vs, es) = vs
edges :: Graph -> [Edge]
edges (vs, es) = es

source :: Edge -> Vertex
source (u, v, w) = u
target :: Edge -> Vertex
target (u,v,w) = v
weight :: Edge -> Weight
weight (u,v,w) = w
cp :: [[a]] -> [[a]]
cp = foldr op[[]] where op xs yss = [x:ys|x <- xs,ys <- yss]