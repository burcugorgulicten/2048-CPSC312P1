module Play2048 where

-- To run it, try:
-- ghci
-- :load Play2048
-- go

import Game2048
import TreeDict -- from lecture
import System.IO
import System.Random
import Data.List

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
    | x == 0 = "*    " ++ y
    | x < 10 = show x ++ "    " ++ y
    | x < 100 = show x ++ "   " ++ y
    | x < 1000 = show x ++ "  " ++ y
    | otherwise = show x ++ " " ++ y

go :: IO Integer
go =
    do
        putStrLn "Enter a username to begin:"
        line <- getLine
        menu (fixdel line) emptyDict

menu :: [Char] -> Dict [Char] Integer -> IO Integer
menu user dict = 
    do
        putStrLn "\nMenu\n1 - regular\n2 - challenge\n3 - leaderboard\n\nEnter a menu item or 'q' to quit"
        option <- getLine
        if fixdel option == "q"
            then
                return 0
            else do
                board <- initboard
                newdict <- start option user board dict
                menu user newdict

start :: [Char] -> [Char] -> [[Integer]] -> Dict [Char] Integer -> IO (Dict [Char] Integer)
start option user board dict
    | option == "1" = do
        score <- play (ContinueGame (State board 0))
        return (insertval user score dict)
    | option == "2" = do
        -- todo: set challenge params
        result <- playChallenge (ContinueGameChallenge (ChallengeState board 0 []))
        return dict
    | option == "3" = do
        putStrLn (leaderboard dict)
        return dict
    | otherwise = return dict

play :: Result -> IO Integer
play (ContinueGame (State board score)) =
   do
      putStrLn ("\nScore: "++show score++display board++"Choose a direction (w,a,s,d):")
      dir <- getLine
      if fixdel dir `elem` ["w", "a", "s", "d"]
        then do
            play (game2048 (head (fixdel dir)) (State board score))
        else do
            putStrLn ("Illegal move: "++ fixdel dir)
            play (ContinueGame (State board score))

play (EndOfGame (State board score) won)
    | won = do
        putStrLn (display board++"You win! Score: "++show score)
        return score
    | otherwise = do
        putStrLn "Game over"
        return score

playChallenge :: ChallengeResult -> IO Integer
playChallenge (ContinueGameChallenge (ChallengeState board moves tiles)) =
   do
      putStrLn ("\n"++display board++"Choose a direction (w,a,s,d):")
      dir <- getLine
      if fixdel dir `elem` ["w", "a", "s", "d"]
        then do
            playChallenge (game2048challenge (head (fixdel dir)) (ChallengeState board moves tiles))
        else do
            putStrLn ("Illegal move: "++ fixdel dir)
            playChallenge (ContinueGameChallenge (ChallengeState board moves tiles))

playChallenge (EndOfGameChallenge (ChallengeState board moves tiles))
    | moves == -1 = do
        putStrLn (display board++"Game over")
        return 0
    | otherwise = do
        putStrLn "You win!"
        return 1

-- leaderboard dict shows a leaderboard with the top usernames and scores
leaderboard :: Dict [Char] Integer -> [Char]
leaderboard dict = "Leaderboard\n" ++ foldr showpair "" (top5 dict)

-- showpair (k,v) r formats the pair and appends it to r
showpair :: ([Char], Integer) -> [Char] -> [Char]
showpair (k,v) r = k ++ ": " ++ show v ++ "\n" ++ r

-- sortByScore dict returns a sorted list of pairs with highest 5 scores in dict
top5 :: Dict [Char] Integer -> [([Char], Integer)]
top5 dict = take 5 (sortBy (\ (_,v1) (_,v2) -> compare v2 v1) (tolist dict))

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

-- testing values
test_dict = insertval "p1" 1234 (insertval "p2" 5432 (insertval "p3" 2111 emptyDict))
bw = [[1024,1024,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
bl = [[4,32,256,8],[16,4,64,4],[4,32,16,8],[2,8,4,0]]