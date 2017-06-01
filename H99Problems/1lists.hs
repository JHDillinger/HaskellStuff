--https://wiki.haskell.org/99_questions/1_to_10

main = print ""

--1
myLast :: [a] -> a
myLast []   = error "Empty list"
myLast [x]  = x
myLast list = head (reverse list)

--2
myButLast :: [a] -> a
myButLast []   = error "Empty list"
myButLast [x]  = x
myButLast list = list !! (length list - 2)

--3
elementAt :: [a] -> Int -> a
elementAt [] k = error "Empty list"
elementAt list k
    | k > length list = error "k larger than length of list"
    | otherwise = list !! (k-1)

--4
myLength :: [a] -> Int
myLength []   = 0
myLength list = 1 + myLength (tail list)


--5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

--6 Find out whether a list is a palindrome.
-- obviously inefficient and long compared to
-- isPalindrome list = list == (reverse list)
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []     = True
isPalindrome [_]    = True
isPalindrome (x:xs) =
    x == head (reverse xs) && isPalindrome (tail $ reverse xs)

--7 Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List [])     = []
flatten (Elem x)      = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

--8 Eliminate consecutive duplicates of list elements.
-- shortest solution: compress = map head . group
-- using Data.List.group

compress :: Eq a => [a] -> [a]
compress []     = []
-- compress [x]    = [x]
compress (x:xs)
    | x == head xs      = compress xs
    | otherwise         = x: compress xs
    --         if x == head xs
    --     then compress xs
    -- else x:compress xs

--9 Pack consecutive duplicates of lists elements into sublists
-- if a list contains repeated elements
-- they should be placed in separate sublists
-- had to look at the solutions and chose the two most "understandable" ones
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) =
    --is x an element of the list @ the head of (pack xs)?
    if x `elem` head (pack xs)
        --then add x to the head list and cons with the rest
        then (x:head (pack xs)):tail (pack xs)
    else [x]:pack xs

-- cons x with a list that takes all x from xs
-- then cons this list with the recursive call of pack'
-- for the rest of elements in xs which are not xs
pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x : takeWhile (==x) xs) : pack' (dropWhile (==x) xs)

--10 Run-length encoding of a list.
--Use the result of P09
--to implement the run-length encoding data compression method.
--Consecutive duplicates of elements are encoded as lists (N E)
--where N is the number of duplicates of the element E.

encode :: Eq a => [a] -> [(Int, a)]
encode list = map (\x -> (length x, head x)) p
    where p = pack list

-- the alternatives which are most "understandable" for me atm:
-- a)
-- encode list = [(length x, head x) | x <- pack list]
-- b)
-- encode :: Eq a => [a] -> [(Int, a)]
-- encode = map (\x -> (length x, head x)) . group
-- c)
-- encode xs = (enc . pack) xs
--      where enc = foldr (\x acc -> (length x, head x) : acc) []
