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

--13 Run-length encoding of a list (direct solution)
-- had to look at the solutions :/

-- encode folds the list with helper
-- it takes the LAST element and writes it as (1,x)
-- at the beginning of the resulting list?!
-- then if the next element is also x, it increments 1
-- how does helper work? isn't the tail ys of type [a]
-- and the head of typ [(Int, a)]?
-- Edit: Ok, it has sth to do with how foldr works...
encode :: Eq a => [a] -> [(Int, a)]
encode = foldr helper []
    where
        helper x [] = [(1,x)]
        helper x (y@(a,b):ys)
            | x == b    = (1+a,x):ys
            | otherwise = (1,x):y:ys

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map encodeHelper . encode
--alternatives:
-- encodeDirect list = map encodeHelper (encode list)
-- encodeDirect list = map encodeHelper $ encode list
    where
        encodeHelper (1,x) = Single x
        encodeHelper (n,x) = Multiple n x

-- 14 duplicate the elements of a list
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x:x:dupli xs

-- 15 replicate the elements of a list a given number of times
-- didn't know I was allowed to use replicate
-- efficiency?
repli :: [a] -> Int -> [a]
repli [] _   = []
repli x 1    = x
repli list n = concatMap (\x -> x : repli [x] (n-1)) list
-- elegant:
-- repli xs n = xs >>= replicate n

-- 16 drop every n'th element from a list
dropEvery :: Eq a => [a] -> Int -> [a]
dropEvery list n = if n > length list
                    then list
                    else x ++ dropEvery y n
                        where
                            x = take (n-1) list
                            y = drop n list

-- 17 split a list into two parts;
-- length of the first part is given
-- solution with predefined predicates:
-- split list n = (take n list, drop n list)
-- had to look at the solution for "without predefined predicates" :/
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split l@(x : xs) n = if n > 0
                        then (x:ys, zs)
                        else ([], l)
        where (ys, zs) = split xs (n-1)

-- 18 elements between i and k (including i and k)
slice :: [a] -> Int -> Int -> [a]
slice list i k = take (k-i+1) $ drop (i-1) list

-- 19 rotate a list N places to the left (hint: use length and ++)
rotate :: [a] -> Int -> [a]
rotate list n = if n > 0
                then drop n list ++ take n list
                else drop (length list + n) list ++ take (length list + n) list

-- 20 remove the k'th element from a list
removeAt :: Int -> [a] -> (a,[a])
removeAt k list = (list !! (k-1), rest)
            where rest = take (k-1) list ++ drop k list 
