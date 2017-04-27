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
isPalindrome [x]    = True
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

compress ::Eq a => [a] -> [a]
compress []     = []
compress [x]    = [x]
compress (x:xs) =
    if x == (head xs)
        then compress xs
    else x:(compress xs)
