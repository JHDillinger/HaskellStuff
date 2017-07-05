import Data.List

main :: IO ()
main = putStrLn "hello, world"

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x ->
        if take nlen x == needle then True
        else acc) False (tails haystack)


secondHead [] = error "empty list"
secondHead [x] = error "only one element"
secondHead (x:y:ys) = y

a >+> b = a+b

test a b = do
    a <- b+1
    a <- a+2
    return a
