-- the countdown problem
-- example: (25-10) * (50+1) = 765
-- eval (App Mul
--          (App Sub (Val 25) (Val 10))
--          (App Add (Val 50) (Val 1))
--       )

-- Für selbst definierte Datentypen
-- vgl überschreiben der "toString" methode in Java
-- haskell muss gesagt werden, wie die datentypen dargestellt
-- werden sollen
instance Show Op where
   show Add           = "+"
   show Sub           = "-"
   show Mul           = "*"
   show Div           = "/"

instance Show Expr where
   show (Val n)       = show n
   show (App o l r)   = bracket l ++ show o ++ bracket r
                        where
                           bracket (Val n) = show n
                           bracket e       = "(" ++ show e ++ ")"

data Op = Add | Sub | Mul | Div

-- apply an operator
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

--decide if the result of applying an operator to two integers
-- greater than zero is another such
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

data Expr = Val Int | App Op Expr Expr

--Return a list of all the values in an expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

--return the overall value of an expression,
-- provided that it is an integer greater than zero
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y]

--alle möglichkeiten, Null bis n elemente aus einer Liste auszuwählen
subbags :: [a] -> [[a]]
subbags xs = [zs | ys <- subs xs, zs <- perms ys]

-- alle sublists einer list
-- [1,2] => [[],[2],[1],[1,2]]
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = ys ++ map (x:) ys
              where
                ys = subs xs

-- alle permutationen der elemente in einer liste
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- alle möglichkeiten, ein einzelnes element in eine liste einzufügen
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- decide if an expression is a solution for a given list
-- of source numbers and a target number
-- Beispiel:
-- solution
--        (App Mul (App Sub (Val 25) (Val 10)) (App Add (Val 50) (Val 1)))
--        [1,3,7,10,25,50]
--        765

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (subbags ns)
                  && eval e == [n]

--------------------------------
-- Brute force implementation
--------------------------------

-- return a list of all possible ways of splitting a list
-- into two non-empty parts
nesplit :: [a] -> [([a], [a])]
nesplit = filter ne . split


-- split an array; includes empty list
split :: [a] -> [([a], [a])]
split [] = [([], [])]
split (x:xs) = ([],x:xs) : [(x:ls, rs) | (ls,rs) <- split xs]

-- both lists in tuple not empty?
ne :: ([a], [b]) -> Bool
ne (xs, ys) = not (null xs || null ys)

-- return a list of all possible expressions whose values are precisely
-- a given list of numbers
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- nesplit ns
              , l       <- exprs ls
              , r       <- exprs rs
              , e       <- combine l r]

-- combine two expressions using each operator
combine :: Expr -> Expr -> [Expr]
combine l r =
    [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- return a list of all possible expressions that solve
-- an instance of the countdown problem
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | nx <- subbags ns
                    , e   <- exprs nx
                    , eval e == [n]]

---------------------------------------------------
-- Applying Program Fusion
-- Fusing generation and evaluation

---------------------------------------------------
type Result = (Expr, Int)

-- bekommt eine list von Ints
-- Gibt eine list von Results aus
-- d.h. Menge an möglichen Expressions und deren Lösung
-- Reihenfolge der Int in der list wird beachtet
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- nesplit ns
                  , lx      <- results ls
                  , ry      <- results rs
                  , res     <- combine' lx ry]


combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

-- return a list of all possible expressions that solve
-- an instance of the countdown problem

solutions'      :: [Int] -> Int -> [Expr]
solutions' ns n =
    [e | ns'    <- subbags ns
        , (e,m) <- results ns'
        , m == n]
-------------------------------------------------
-- exploiting arithmetic properties
-------------------------------------------------

-- strengthening the valid predicate to take account of
-- commutativity and identity properties
valid'          :: Op -> Int -> Int -> Bool
valid' Add x y  = x <= y
valid' Sub x y  = x > y
valid' Mul x y  = x /= 1 && y /= 1 && x <= y
valid' Div x y  = y /= 1 && x `mod` y == 0

eval'           :: Expr -> [Int]
eval' (Val n)   = [n | n > 0]
eval' (App o l r) = [apply o x y | x <- eval' l, y <- eval' r, valid' o x y]

solution'       :: Expr -> [Int] -> Int -> Bool
solution' e ns n= elem (values e) (subbags ns) && eval' e == [n]

results'        :: [Int] -> [Result]
results' []     = []
results' [n]    = [(Val n,n) | n > 0]
results' ns           = [res | (ls,rs) <- nesplit ns
                             , lx      <- results' ls
                             , ry      <- results' rs
                             , res     <- combine'' lx ry]


combine''       :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions''     :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | ns' <- subbags ns, (e,m) <- results' ns', m == n]
