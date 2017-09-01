module Main where

main :: IO ()
main = putStrLn "Hello World"

split :: [a] -> [(a, [a])]
split []     = []
split (x:xs) = (x, xs) : [(y, x : ys) | (y, ys) <- split xs]

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [v : p | (v, vs) <- split (id xs), p <- perms vs]

-- list is smooth if difference of consecutive elements
-- is <= n
smooth :: (Num a, Ord a) => a -> [a] -> Bool
smooth n (x:y:ys) = abs (y - x) <= n && smooth n (y : ys)
smooth _ _        = True

smooth_perms :: Int -> [Int] -> [[Int]]
smooth_perms n xs = filter (smooth n) (perms xs)

-- build a tree:
-- Each path from root to leaf: One possible permutation
-- prune the tree: only smooth paths
-- use the tree to generate all smooth permutations
data PermTree a =
  Node Int
       [PermTree Int]
  deriving (Show, Eq)

testtree1 = Node 3 [Node 2 [Node 1 []], Node 2 [Node 3 []]]

testtree2 = Node 2 [Node 1 []]

createPermTrees :: (Num a, Ord a) => [Int] -> [PermTree a]
createPermTrees [] = []
createPermTrees xs = [Node e (createPermTrees r) | (e, r) <- createRootPerm xs]

-- create all combinations of Tuples of (x, rest) from a list
createRootPerm :: (Num a, Ord a) => [a] -> [(a, [a])]
createRootPerm [] = []
createRootPerm xs = map (\x -> (x, removeItem x xs)) xs

removeItem :: (Num a, Ord a) => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys)
  | x == y = removeItem x ys
  | otherwise = y : removeItem x ys

-- get all smooth permutations from a list of PermTrees
-- first: how to get all smooth permutations from one PermTree?
-- maybe use list comprehension?
-- get the first smooth permutation from a given Tree
-- but what if there are more than one?
getSmoothPerm :: (Num a, Ord a) => Int -> PermTree a -> [[Int]]
getSmoothPerm _ (Node a []) = [[a]]

-- checks smoothness of root node and second level nodes
-- of a given PermTree
-- Probably to be called in every step to know which paths to keep following?
smooth' :: (Num a, Ord a) => Int -> PermTree a -> [Bool]
smooth' n (Node a xs) = map (helpsmooth n a) xs
  where
    helpsmooth n' i (Node a' _) = abs (i - a') <= n'

---------------------------------------------------------------
-- get all paths from root to leaves
-- kinda useless because I would have to filter again for smoothness
-- no benefit to original
getPaths :: (Num a, Ord a) => Int -> PermTree a -> [[Int]]
getPaths _ (Node a []) = [[a]]
getPaths n (Node a xs) =
  filter (smooth n) (map (a :) $ concat $ map (getPaths n) xs)

getSmoothPerms' :: (Num a, Ord a) => Int -> [PermTree a] -> [[Int]]
getSmoothPerms' n xs = concatMap (getPaths n) xs
