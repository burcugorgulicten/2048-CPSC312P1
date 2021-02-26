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
import System.Directory

go :: IO Integer
go =
    do
        putStrLn "Enter a username to begin:"
        line <- getLine
        dict <- loadDict
        main (fixdel line) dict

-- main menu
menu = "\nMenu\n1 - New game\n2 - New challenge game\n3 - Display leaderboard\n\nEnter a menu item or 'q' to quit"

main :: [Char] -> Dict [Char] Integer -> IO Integer
main user dict = 
    do
        putStrLn menu
        option <- getLine
        if fixdel option == "q"
            then do
                saveDict dict 
                return 0
            else do
                newdict <- start (fixdel option) user dict
                main user newdict

start :: [Char] -> [Char] -> Dict [Char] Integer -> IO (Dict [Char] Integer)
-- start regular game
start "1" user dict =
    do
        board <- initboard
        score <- play (ContinueGame (State board 0)) (getHighscore user dict)
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
start _ user dict = 
    do
        putStrLn "Invalid menu option"
        return dict

play :: Result -> Integer -> IO Integer
play (ContinueGame (State board score)) highscore =
   do
      putStrLn ("\nScore: "++show score++" Best: "++show (max score highscore)++display board++"Choose a direction (w,a,s,d):")
      dir <- getLine
      if fixdel dir `elem` ["w", "a", "s", "d"]
        then do
            play (game2048 (head (fixdel dir)) (State board score)) highscore
        else do
            putStrLn ("Illegal move: "++ fixdel dir)
            play (ContinueGame (State board score)) highscore

play (EndOfGame (State board score) won) highscore
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


------------ Board Functions -------------

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

-- helper for display - displaynum n s appends n to s with spacing
displaynum :: Integer -> String -> String
displaynum n s
    | n == 0 = "*    " ++ s
    | n < 10 = show n ++ "    " ++ s
    | n < 100 = show n ++ "   " ++ s
    | n < 1000 = show n ++ "  " ++ s
    | otherwise = show n ++ " " ++ s


------------ Dict Functions -------------

-- leaderboard dict shows a leaderboard with the top scores in dict
leaderboard :: Dict [Char] Integer -> [Char]
leaderboard dict
    | null (tolist dict) = "\nno scores to display"
    | otherwise = "\nLeaderboard\n" ++ foldr showpair "" (top5 dict)

-- helper for leaderboard - showpair (k,v) r formats the pair and appends it to r
showpair :: ([Char], Integer) -> [Char] -> [Char]
showpair (k,v) r = k ++ ": " ++ show v ++ "\n" ++ r

-- top5 dict returns a sorted list of pairs with highest 5 scores in dict
top5 :: Dict [Char] Integer -> [([Char], Integer)]
top5 dict = take 5 (sortBy (\ (_,v1) (_,v2) -> compare v2 v1) (tolist dict))

-- loadDict reads the dictionary from scores.txt
loadDict :: IO (Dict [Char] Integer)
loadDict =
    do
        scoresExist <- doesFileExist "scores.txt"
        if scoresExist
            then do
                contents <- readFile "scores.txt"
                let pairs = makepairs (lines contents)
                return (foldr (uncurry insertval) emptyDict pairs)
            else
                return emptyDict

-- helper for loadDict - makepairs lst returns a list of key value pairs in lst
makepairs :: [[Char]] -> [([Char], Integer)]
makepairs [] = []
makepairs (k:v:t) = (k, read v) : makepairs t

-- saveDict writes the dictionary to scores.txt
saveDict :: Dict [Char] Integer -> IO ()
saveDict dict = writeFile "scores.txt" (foldr (\ (k,v) y -> k++"\n"++show v++"\n"++y) "" (tolist dict))

-- getHighscore user dict returns user's highscore
getHighscore :: [Char] -> Dict [Char] Integer -> Integer
getHighscore user dict = convertScore (getval user dict)

-- helper for getHighscore
convertScore :: Maybe Integer -> Integer
convertScore Nothing = 0
convertScore (Just score) = score

-- note: fixdel is from the Assignment 3 solutions
-- fixdel removes deleted elements from string
fixdel :: [Char] -> [Char]
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel :: [Char] -> [Char]
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r


------------ Testing Values -------------

test_dict = insertval "p1" 1234 (insertval "p2" 5432 (insertval "p3" 2111 emptyDict))
bw = [[1024,1024,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
bl = [[4,32,256,8],[16,4,64,4],[4,32,16,8],[2,8,4,0]]