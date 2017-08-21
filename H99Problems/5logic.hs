main :: IO ()
main = print "Hallo"

-- 46

not' :: Bool -> Bool
not' True  = False
not' False = True

and', or', nor', xor', impl', equ', nand':: Bool -> Bool -> Bool
and' True True = True
and' _ _       = False

or' False False = False
or' _ _         = True

nand' x y = not' (and' x y)

nor' x y = not' (or' x y)


xor' True False = True
xor' False True = True
xor' _ _ = False

impl' x y = not' x `or'` y

equ' True True = True
equ' False False = True
equ' _ _ = False


table :: (Bool -> Bool -> Bool) -> IO()
table f =   putStrLn $ unlines
            [show a ++ " " ++ show b ++ " " ++ show (f a b)
            | a <- [True, False], b <- [True, False]]
