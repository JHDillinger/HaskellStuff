import Data.List (nub)
-- import Data.Ratio


main :: IO ()
main = print "test"

--------------------------------------------------------------------------------
-- 31 Determine whether a given int is prime
-- I don't really know why the solutions on wiki.haskell are so complicated
-- is this solution too inefficient? idk
isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

factors n = [y | y <- [2..n], n `mod` y == 0]
--------------------------------------------------------------------------------
-- 32 greatest common divisor using euclid's algorithm
myGCD :: Int -> Int -> Int
myGCD a b
        | b == 0    = abs a
        | otherwise = myGCD b (a `mod` b)

--------------------------------------------------------------------------------
-- 33  Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

--------------------------------------------------------------------------------
-- 34 Calculate Euler's totient function phi(m)
-- Euler's so-called totient function phi(m) is defined as
-- the number of positive integers r (1 <= r < m) that are coprime to m.

-- this solution is very slow for large numbers
totient :: Int -> Int
totient m = length [r | r <- [1..m], coprime r m]

-- totient' :: Integral a => a -> a
-- totient' 1 = 1
-- totient' n = numerator ratio `div` denominator ratio
--     where
--         ratio = foldl (\acc x -> acc * (1- (1%x)))
--                     (n % 1) $ nub (primeFactors n)

--------------------------------------------------------------------------------
-- 35 generate a list of the prime factors of a given positive integer
-- again: almost had it but only for the first step of dividing by 2
-- and couldn't fit it all together
primeFactors :: Int -> [Int]
-- primeFactors n = primeFactors' n 2
--     where
--         primeFactors' 1 _ = []
--         primeFactors' n f
--             | f*f > n           = [n]
--             | n `mod` f == 0    = f : primeFactors' (n `div` f) f
--             | otherwise         = primeFactors' n (f+1)

-- alternative:
primeFactors 1  = []
primeFactors n  = p : primeFactors (n `div` p)
                    where p = head $ dropWhile ((/= 0) . mod n) [2..n]
-- let prime = head $ dropWhile ((/= 0) . mod n) [2..n]
--                 in (prime : ) $ primeFactors $ div n prime

--------------------------------------------------------------------------------
-- 36 prime factors of an Int and count how often they occur
-- using nub from Data.List
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = nub [(x, count x pf) | x <- pf]
    where   count x = length . filter (x==)
            pf = primeFactors n

-- alternatively using group from Data.List
-- prime_factors_mult = map encode . group . primeFactors
--     where encode xs = (head xs, length xs)
-- alternatively using encode from problem 10
-- prime_factors_mult n = map swap $ encode $ primeFactors n
--   where swap (x,y) = (y,x)

--------------------------------------------------------------------------------

-- 37
