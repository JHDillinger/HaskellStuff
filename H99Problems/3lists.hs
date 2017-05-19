import System.Random
import Data.List
import Control.Monad (replicateM)

main :: IO ()
main = print "lists3"


-- 21 insert an element at a given position into a list
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x (y:ys) i =
    if i == 1 then x:y:ys
    else x:insertAt x ys (i-1)

--------------------------------------------------------------------
-- 22 create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range i k = [i..k]

-- oh boy, randoms in haskell...
-- 23 extract a given number f randomly selected elements from a list
-- perform rand n times
-- rand generates random number in Range 0-length-1 to get random index
-- then return the element at that index
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n
    | n < 0     = error "N must be greater than zero."
    | otherwise = replicateM n rand
        where rand = do r <- randomRIO (0, length xs - 1)
                        return (xs !! r)
----------------------------------------------------------------------------
-- 24 Lotto: Draw N different random numbers from the set 1..M
-- following solution generates duplicates
-- diff_select :: Int -> Int -> IO [Int]
-- diff_select n m = replicateM n rand
--         where rand = randomRIO (0, m)

-- The next solution generates NO duplicates BUT only works once
-- because the result will be the same because of the same StdGen
-- diff_select :: Int -> Int -> StdGen -> [Int]
-- diff_select n m = take n . nub . randomRs (1, m)

-- this solution does both...
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = diffSelect' n [1..m]
-- r is random number in range (0, length-1)
-- remaining is: list without element at index r
-- rest: recursive call with n-1 and remaining
-- then return element at index r and cons with rest
diffSelect' 0 _  = return []
diffSelect' _ [] = error "too few elements to choose from"
diffSelect' n xs = do r <- randomRIO (0, length xs - 1)
                      let remaining = take r xs ++ drop (r+1) xs
                      rest <- diffSelect' (n-1) remaining
                      return ((xs!!r) : rest)

----------------------------------------------------------------
-- 25 generate a random permutation of the elements of a list
-- pretty much the same as in problem 24 diffSelect'
-- idk about how good this solution is and if I'd found it without 24
rndPermu :: [a] -> IO [a]
rndPermu [] = return []
rndPermu xs = do r <- randomRIO (0, length xs -1)
                 let remaining = take r xs ++ drop (r+1) xs
                 rest <- rndPermu remaining
                 return ((xs !! r) : rest)

------------------------------------------------------------
-- 26 generate the combinations of K distinct objects
-- chosen from the N elements of a list
-- y is the first element of tails, that means the full List
-- xs' is the rest -> recursive call of combinations with xs'
-- but how does it work? wouldn't the head of tails xs' be shorter by 1 in every
-- step?
-- combinations :: Int -> [a] -> [[a]]
-- combinations 0 _ = return []
-- combinations n xs = do  y:xs' <- tails xs
--                         ys <- combinations (n-1) xs'
--                         return (y:ys)

-- this was the approach I thought of first but also couldn't finish
-- therefore: had to look at the solutions

-- "Get all combinations that start with x, recursively choosing (k-1) from the
-- remaining xs. After exhausting all the possibilities starting with x, if there
-- are at least k elements in the remaining xs, recursively get combinations of k
-- from the remaining xs."
combinations :: Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 0 _  = [[]]
combinations k (x:xs) = x_start ++ others
    where
        x_start = [x : rest | rest <- combinations (k-1) xs]
        others = if k <= length xs then combinations k xs else []



--------------------------------------------------------------
-- 27 group the elements of a set into disjoint subsets
-- all possibilities how to group the items of a list into sublists with given
-- group sizes
-- group' [2,2] "abcd"
-- all possibilities how to make two disjoint sublists of length 2 out of "abcd"
-- no chance :/

-- combination takes an Int and returns a list of tuples of lists
-- the first item in the tuple is a combination of n items
-- the second item is a list of the remaining items
-- together all the first items in the tuples give the results of problem 26
combination :: Int -> [a] -> [([a], [a])]
combination 0 xs    = [([],xs)]
combination n []    = []
combination n (x:xs)= ts ++ ds
    where
        ts = [(x:ys, zs) | (ys, zs) <- combination (n-1) xs]
        ds = [(ys, x:zs) | (ys, zs) <- combination n xs]

-- group takes list of integers (the group sizes) and the list
-- list comprehension g:gs where
-- g is the first item of the tuples from above
-- (combination of n items/first group size of Int list)
-- gs | recursively call group' with the rest of the integerlist (group sizes)
-- applied to rs, i.e. the rest of the items which aren't yet in groups of n
group' :: [Int] -> [a] -> [[[a]]]
group' [] _ = [[]]
group' (n:ns) xs =
    [ g:gs   | (g,rs) <- combination n xs
             , gs     <- group' ns rs]


-- 28 Sorting a list of lists according to length of sublists

-- a) short lists first, longer lists later
-- quick sort with length of the items
lsort :: Ord a => [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) =  lsort [ a | a <- xs, length a <= length x ]
                ++ [x] ++
                lsort [ b | b <- xs, length b > length x]

-- b) sort according to "length frequency"
-- items with rare lengths placed first
-- had to look at the solutions only to realize I had almost found it myself
-- didn't exactly know how to glue it all together
lfsort :: Ord a => [[a]] -> [[a]]
lfsort list = concat $ lsort $ lcomp (lsort list)
                where lcomp = groupBy (\x y -> (length x == length y))
