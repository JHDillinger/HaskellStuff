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
    print (relativesqroot a0 eps n)

-- relativesqrt :: Double -> Double -> Double -> Double
relativesqroot = sqroot relative

-- withinsqroot :: Double -> Double -> Double -> Double
withinsqroot = sqroot within

-- another abstraction layer
sqroot method a0 eps n = method eps (repeat' (next n) a0)

-- like in the paper:
-- sqroot :: (Ord a, Fractional a) => a -> a -> a -> a
-- sqroot a0 eps n = within eps (nrepeat (next n) a0)

-- relativesqroot :: (Ord a, Fractional a) => a -> a -> a -> a
-- relativesqroot a0 eps n = relative eps (nrepeat (next n) a0)


-- creates a_i+1 from a_i (no "general purpose" function; only for this example)
next :: (Fractional a) => a -> a -> a
next n x = (x + n/x)/2

-- creates an infinite array of repeated applications of f
-- implemented as iterate in Prelude
repeat' :: (a -> a) -> a -> [a]
repeat' f a = a : repeat' f (f a)

within :: (Num a, Ord a) => a -> [a] -> a
within _ [] = 0
within _ [x] = x
within eps (a:b:rest)   | abs (a-b) <= eps = b
                        | otherwise = within eps (b:rest)

relative :: (Fractional a, Ord a) => a -> [a] -> a
relative _ [] = 0
relative _ [x] = x
relative eps (a:b:rest) | abs (a/b-1) <= eps = b
                        | otherwise = relative eps (b:rest)

test :: a -> [a] -> a
test eps list = eps
