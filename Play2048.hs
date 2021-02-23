module Play2048 where

import Game2048
import TreeDict -- from lecture
import System.IO
import System.Random

emptyboard = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]

initboard = 
    do
        num <- randomRIO (1, 2 :: Integer)
        return (addTile (addTile emptyboard 1) num)

display :: [[Integer]] -> String
display = foldr (\ x y -> show x ++ "\n" ++ y) []

start :: [[Integer]] -> Dict [Char] Integer -> IO (Dict [Char] Integer)
start board dict =
   do
      putStrLn "Enter a username to play or 'q' to quit:"
      line <- getLine
      if line == "q"
        then 
            return dict
        else do
            score <- play (ContinueGame (State board 0))
            newboard <- initboard
            start newboard (insertval line score dict)

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

go :: IO (Dict [Char] Integer)
go = 
    do 
        board <- initboard
        start board emptyDict