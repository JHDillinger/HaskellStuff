module Main where

main :: IO ()
main = putStrLn "Hello World"

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

splitleft :: Tree a -> (a, Maybe (Tree a))
splitleft (Leaf a)      = (a, Nothing)
splitleft (Node l r)    = case splitleft l of
                            (a, Nothing) -> (a, Just r)
                            (a, Just l') -> (a, Just (Node l' r))

-- gesucht: tail-recursive variant of splitleft
-- Hint: generalize splitleft by introducing additional auxiliary parameter
testtree1 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))
