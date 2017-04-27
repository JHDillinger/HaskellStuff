-- newton-raphson square roots from
-- "why functional programming matters"

main :: IO()
main = do
    putStrLn "Enter the number for which you want to calculate sqrt:"
    n <- readLn
    putStrLn "Enter the start value:"
    a0 <- readLn
    putStrLn "Enter the eps value:"
    eps <- readLn
    putStrLn "The approximated square root is:"
    print (sqroot a0 eps n)

sqroot :: (Ord a, Fractional a) => a -> a -> a -> a
sqroot a0 eps n = within eps (nrepeat (next n) a0)

relativesqroot :: (Ord a, Fractional a) => a -> a -> a -> a
relativesqroot a0 eps n = relative eps (nrepeat (next n) a0)

next :: (Fractional a) => a -> a -> a
next n x = (x + n/x)/2

nrepeat :: (a -> a) -> a -> [a]
nrepeat f a = a:nrepeat f (f a)

within :: (Num a, Ord a) => a -> [a] -> a
within _ [] = 0
within eps (a:b:rest)
    | abs (a-b) <= eps = b
    | otherwise = within eps (b:rest)

relative :: (Fractional a, Ord a) => a -> [a] -> a
relative _ [] = 0
relative eps (a:b:rest)
    | abs (a/b-1) <= eps = b
    | otherwise = relative eps (b:rest)
