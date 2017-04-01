-- nim
-- two players take turns and remove one or more stars
-- from the end of a single row
-- the winner is the player who removes the last star or stars
-- from the board

board :: [Int]
board = [5,4,3,2,1]

main :: IO ()
main = game board

-- "main" function for the game; user input,
-- shows changes on the board etc
game :: [Int] -> IO ()
game list = if all (==0) list
               then do
                 showBoard list
                 putStrLn "The last player won the game!"
               else do
                 showBoard list
                 putStrLn "Choose line!"
                 l <- readLn
                 putStrLn "Choose how many stars to remove!"
                 s <- readLn
                 remove (list, l, s)

-- remove s stars from line l
remove :: ([Int], Int, Int) -> IO ()
remove (list, l, s) = if (s > list!!(l-1)) || (s<0)
                        then game list
                      else game (take (l-1) list
                                ++ [(list!!(l-1)) - s]
                                ++ drop l list)

-- shows the line of stars
showLine :: Int -> IO ()
showLine 0 = putChar '\n'
showLine x = do putChar '*'
                showLine (x-1)

-- shows the complete board with line numbers
showBoard :: [Int] -> IO ()
showBoard [] = return ()
showBoard (x:xs) = do putStr (show (length board - length xs)
                                    ++ ": ")
                      showLine x
                      showBoard xs
