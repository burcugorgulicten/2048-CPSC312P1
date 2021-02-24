module Play2048 where

-- To run it, try:
-- ghci
-- :load Play2048
-- go

import Game2048
import TreeDict -- from lecture
import System.IO
import System.Random

emptyboard = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]

-- initboard creates a new board with 2 random tiles
initboard = 
    do
        num <- randomRIO (1, 2 :: Integer)
        return (addTile (addTile emptyboard 1) num)

-- display b prints the board with proper spacing
display :: [[Integer]] -> String
display b = "\n" ++ foldr (\ x y -> foldr displaynum "" x ++ "\n" ++ y) "\n" b

displaynum :: Integer -> String -> String
displaynum x y
    | x < 10 = show x ++ "    " ++ y
    | x < 100 = show x ++ "   " ++ y
    | x < 1000 = show x ++ "  " ++ y
    | otherwise = show x ++ " " ++ y

start :: [[Integer]] -> Dict [Char] Integer -> IO (Dict [Char] Integer)
start board dict =
   do
      putStrLn "Enter a username to play or 'q' to quit:"
      line <- getLine
      if fixdel line == "q"
        then 
            return dict
        else do
            score <- play (ContinueGame (State board 0))
            newboard <- initboard
            start newboard (insertval (fixdel line) score dict)

play :: Result -> IO Integer
play (ContinueGame (State board score)) =
   do
      putStrLn (display board++"Choose a direction:")
      dir <- getLine
      if fixdel dir `elem` ["w", "a", "s", "d"]
        then do
            play (game2048 (head (fixdel dir)) (State board score))
        else do
            putStrLn ("Illegal move: "++ fixdel dir)
            play (ContinueGame (State board score))

play (EndOfGame (State board score) won)
    | won = do
        putStrLn "You win!"
        return score
    | otherwise = do
        putStrLn "Game over"
        return score

go :: IO (Dict [Char] Integer)
go = 
    do 
        board <- initboard
        start board emptyDict

-- note: the following function was taken from the Assignment 3 solutions
-- fixdel removes deleted elements from string
fixdel :: [Char] -> [Char]
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel :: [Char] -> [Char]
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r