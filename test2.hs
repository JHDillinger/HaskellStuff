import           Data.List


main :: IO ()
main = putStrLn "hello, world"

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x ->
        if take nlen x == needle then True
        else acc) False (tails haystack)


secondHead []       = error "empty list"
secondHead [x]      = error "only one element"
secondHead (x:y:ys) = y

a >+> b = a+b

-- f :: Int -> Int
-- f x = "test"  + 1

-- example2 :: Integer
-- example2 = foldr (+) 0 tree


test a b = do
    a <-  b+1
    a <-   a+2
    return a


nameDo :: IO ()
nameDo = do putStr "What is your first name? "
            first <- getLine
            putStr "And your last name? "
            last' <- getLine
            let full = first ++ " " ++ last'
            putStrLn ("Pleased to meet you, " ++ full ++ "!")
            last' <- getLine
            putStrLn last'
            putStrLn ("Pleased to meet you, " ++ full ++ "!")
