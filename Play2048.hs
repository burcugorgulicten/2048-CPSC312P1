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

-- starting board with no tiles
emptyboard = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]

-- initboard creates a new board with 2 random tiles
initboard :: IO [[Integer]]
initboard = 
    do
        num <- randomRIO (1, 2 :: Integer)
        return (addTile (addTile emptyboard 1) num)

-- initboardchallenge board n m s creates a new challenge board
-- with n normal tiles, m movable tiles, and s static tiles 
initboardchallenge :: [[Integer]] -> [Integer] -> Integer -> Integer -> IO [[Integer]]
initboardchallenge board n m s 
    | n /= [] = initboardchallenge (addTile board (head n)) (tail n) m s
    | m > 0 = initboardchallenge (addTile board (-1)) n (m-1) s
    | s > 0 = initboardchallenge (addTile board (-2)) n m (s-1)
    | otherwise = return board

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
        main (fixdel line) emptyDict

menu = "\nMenu\n1 - New game\n2 - New challenge game\n3 - Display leaderboard\n\nEnter a menu item or 'q' to quit"

main :: [Char] -> Dict [Char] Integer -> IO Integer
main user dict = 
    do
        putStrLn menu
        option <- getLine
        if fixdel option == "q"
            then
                return 0
            else do
                newdict <- start option user dict
                main user newdict

start :: [Char] -> [Char] -> Dict [Char] Integer -> IO (Dict [Char] Integer)
-- start regular game
start "1" user dict =
    do
        board <- initboard
        score <- play (ContinueGame (State board 0))
        return (insertval user score dict)

-- start challenge game
start "2" user dict =
    do
        let (ChallengeGameEnv moves tiles n movable static) = head challenge_games
        board <- initboardchallenge emptyboard n movable static
        result <- startChallenge board moves tiles 0
        return dict

-- show leaderboard
start "3" user dict =
    do
        putStrLn (leaderboard dict)
        return dict

-- invalid menu option
start _ user dict = return dict

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

startChallenge:: [[Integer]] -> Integer -> [(Integer, Integer)] -> Integer -> IO Integer
startChallenge board moves tiles index =
   do
       cur_index <- playChallenge (ContinueGameChallenge (ChallengeState board moves tiles index))
       let (ChallengeGameEnv new_moves new_tiles n movable static) = challenge_games !! fromIntegral cur_index
       putStrLn "Would you like to continue? 'yes', 'no'"
       continue <- getLine 
       if fixdel continue == "yes"
           then do
               new_board <- initboardchallenge emptyboard n movable static
               startChallenge new_board new_moves new_tiles cur_index
            else
                return cur_index

playChallenge :: ChallengeResult -> IO Integer
playChallenge (ContinueGameChallenge (ChallengeState board moves tiles index)) =
   do
      putStrLn ("\nMoves Left: "++show moves++"\nTiles: "++show tiles++display board++"Choose a direction (w,a,s,d):")
      dir <- getLine
      if fixdel dir `elem` ["w", "a", "s", "d"]
        then do
            playChallenge (game2048challenge (head (fixdel dir)) (ChallengeState board moves tiles index))
        else do
            putStrLn ("Illegal move: "++ fixdel dir)
            playChallenge (ContinueGameChallenge (ChallengeState board moves tiles index))

playChallenge (EndOfGameChallenge (ChallengeState board moves tiles index) won)
    | won = do
        putStrLn (display board++"Challenge complete!")
        if index == fromIntegral (length challenge_games) - 1
            then do 
                putStrLn "Cleared all challenges!"
                return index
            else do
                return (index + 1)
    | otherwise = do
        putStrLn "Game over"
        return index

-- leaderboard dict shows a leaderboard with the top usernames and scores
leaderboard :: Dict [Char] Integer -> [Char]
leaderboard dict
    | tolist dict == [] = "\nno scores to display"
    | otherwise = "\nLeaderboard\n" ++ foldr showpair "" (top5 dict)

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