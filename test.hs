import           Data.Char (isDigit)
--import Data.List

main = print "Hello World!"

isDigitTest      :: Char -> Bool
isDigitTest n    = isDigit n

checkNum  :: String -> Bool
checkNum = all isDigit

add       :: (Int, Int) -> Int
add (x,y) = x+y

zeroto    :: Int -> [Int]
zeroto n  = [0..n]

-- ['a', 'b', 'c'] :: [Char]
-- ('a', 'b', 'c') :: (Char, Char, Char)
-- [(False, '0'), (True, '1')] ::[(Bool, Char)]
-- [isDigit, isLower, isUpper] :: [Char-> Bool]

test        :: (Char, Char) -> [Char]
test (a,b)  = [a, b]

test2       :: (Char, Char, Char) -> (Char, Char, Char)
test2 (a,b,c)      = (c,b,a)

test3       :: (Bool, Char) -> [(Bool, Char)]
test3 (a,b)      = [(a,b), (not a, b)]

-- abs n  | n>= 0 =n
--        | otherwise = -n
-- gleich wie: abs n= if n>= 0 then n else -n
-- heißt: guarded equations sind wie Fallunterscheidungen in Mathe

twoheads :: [a] -> (a,a)
twoheads (x:s:_) = (x,s)

safetail :: [a] -> [a]
safetail a = if null a || length a == 1 then [] else tail a

safetail2 :: [a] -> [a]
safetail2 a | null a || length a == 1 = []
            | otherwise = tail a

safetail3 :: [a] -> [a]
safetail3 []    =[]
safetail3 (_:x) = x


addnew = \x -> (\y -> x+y)

prime :: Int -> Bool
prime n = factors n ==[1,n]

primes :: Int -> [Int]
primes n =[x | x <- [1..n], prime x]

triads :: Int -> [(Int, Int, Int)]
triads n = [(x,y,z) | x <- [1..n], y<-[1..n], z <- [1..n]
                    , z^2 == (x^2 + y^2) ]

factors :: Int-> [Int]
factors n = [x | x <- [1..n-1]
                , n `mod` x==0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n]
                , x == sum (factors x)]

--Recursion----------------------------------------------

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort [a | a<- xs, a <= x]
                ++ [x] ++
                qsort [b | b<- xs, b > x]

insert :: Int -> [Int] -> [Int]
insert i [] = [i]
insert i (x:xs) = if i <= x then i:(x:xs)
                  else x : insert i xs

isort :: [Int] -> [Int]
isort []     = []
isort (x:xs) = insert x (isort xs)

merge :: [Int] -> [Int] -> [Int]
merge [] [] = [] ++ []
merge x [] = x ++ []
merge [] x = x ++ []
merge (a:xs) (b:ys) = if a <= b then a : merge xs (b:ys)
                      else b : merge (a:xs) ys

msort :: [Int] -> [Int]
msort []     = []
msort [x]    = [x]
msort (x:xs) = merge (qsort [x]) (qsort xs)

--Higher Order Functions------------------------------

twice :: (a -> a) -> a -> a
twice f x = f (f x)

--reading two chars from keyboard
getTwo :: IO (Char, Char)
getTwo = do x <- getChar
            y <- getChar
            return (x,y)

--reading a string from the keyboard
sgetLine :: IO String
sgetLine = do x <- getChar
              if x == '\n' then
                 return []
                else
                  do xs <- sgetLine
                     return (x:xs)

sputStr :: String -> IO ()
sputStr [] = return ()
sputStr (x:xs) = do putChar x
                    putStr xs
                    putChar '\n'

strlen :: IO()
strlen  = do putStr "Enter a string: "
             xs <- getLine
             putStr "The String has "
             putStr (show (length xs))
             putStrLn " characters"

-- Data Declarations
data Shape  = Circle Float
            | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Rect x y) = x*y


teststring =    "__|___|__"++ "\n"
            ++  "__|___|__"++ "\n"
            ++  "  |   |  "

testfunction :: [[Int]] -> [Int]
testfunction x = map (negate . sum . tail) x
