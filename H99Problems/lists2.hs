-- Problems 11 to 20 of the 99 Problems in Haskell
-- https://wiki.haskell.org/99_questions/11_to_20
main :: IO ()
main = print "hello"
--11 Modified run-length encoding.
-- Modify the result of problem 10 that if an element has no duplicates
-- it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.

pack :: Eq a => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)


data ListItem a = Multiple Int a | Single a deriving (Show, Eq)

encodeMod :: Eq a => [a] -> [ListItem a]
encodeMod list = map (\x ->
    if length x > 1
        then Multiple (length x) (head x)
    else Single (head x)) p
    where p = pack list

--12 Decode a run-length encoded list
count :: ListItem a -> Int
count (Single _)     = 1
count (Multiple c _) = c

item :: ListItem a -> a
item (Multiple _ a) = a
item (Single a)     = a

decodeMod :: [ListItem a] -> [a]
decodeMod = concatMap (\x -> replicate (count x) (item x))
