module Play2048 where

import Game2048
import System.IO

sb = [[2,0,0,0],[0,0,0,2],[0,2,0,0],[2,0,0,0]]

display :: [[Integer]] -> String
display = foldr (\ x y -> show x ++ "\n" ++ y) []

start :: [[Integer]] -> IO Integer
start board =
   do
      putStrLn "Enter a username to play or 'q' to quit:"
      line <- getLine
      if line == "q"
        then 
            return 0
        else do
            play (ContinueGame (State board 0))

play :: Result -> IO Integer
play (ContinueGame (State board score)) =
   do
      putStrLn (display board++"Choose a direction:")
      dir <- getLine
      if dir `elem` ["w", "a", "s", "d"]
        then do
            play (game2048 (head dir) (State board score))
        else do
            putStrLn ("Illegal move: "++ dir)
            play (ContinueGame (State board score))

play (EndOfGame (State board score))
    | score == 0 = do
        putStrLn "You lose"
        return score
    | otherwise = do
        putStrLn "You win!"
        return score

go :: IO Integer
go = start sb