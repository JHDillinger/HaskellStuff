module Main where

main :: IO ()
main = putStrLn "Hello World"

data Tree a =   Empty | Branch a (Tree a) (Tree a)
                deriving (Show, Eq)


-- Problem 55 Construt completely balanced binary trees
-- difference between number of nodes
-- in left and right subtree <=1
-- given number of nodes

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n-1) `quotRem` 2
    in [Branch 'x' left right | i       <- [q .. q +r],
                                left    <- cbalTree i,
                                right   <- cbalTree (n - i -1)]

-- 56 symmetric binary trees
-- is a given binary tree symmetric?
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty                   = True
mirror (Branch _ a b) (Branch _ x y)=mirror a y && mirror b x
mirror _ _                           = False


symmetric :: Tree a -> Bool
symmetric Empty           = True
symmetric  (Branch _ l r) = mirror l r

-- 57 binary search trees

add :: Ord a => a -> Tree a -> Tree a
add x Empty            = Branch x Empty Empty
add x t@(Branch y l r) = case compare x y of
                            LT -> Branch y (add x l) r
                            GT -> Branch y l (add x r)
                            EQ -> t

construct xs = foldl (flip add) Empty xs


-- 61 count the leaves of a binary tree
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

countLeaves :: Tree a -> Int
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r)         = countLeaves l + countLeaves r

-- 61A collect the leaves of a binary tree in a list
leaves :: Tree a -> [a]
leaves Empty                  = []
leaves (Branch a Empty Empty) = [a]
leaves (Branch _ l r)         = leaves l ++ leaves r

-- 62 collect the internal nodes of a binary tree in a list
internals :: Tree a -> [a]
internals Empty                  = []
internals (Branch _ Empty Empty) = []
internals (Branch a l r)         = a : internals l ++ internals r

-- 62B collect the nodes at a given level in a list
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch a l r) n
        | n == 1    = [a]
        | n > 1     = atLevel l (n-1) ++ atLevel r (n-1)
        | otherwise = []
