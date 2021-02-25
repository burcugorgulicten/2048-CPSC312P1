module Game2048 where
import System.Random
import System.IO.Unsafe


b  :: [[Integer]]
b = [[2,0,0,0],[0,0,0,2],[0,2,0,0],[2,0,0,0]]


static_block :: Integer
static_block = -4
movable_block :: Integer
movable_block = -2

------------ Definitions -------------

data State = State [[Integer]] Integer
        deriving (Ord, Eq, Show)


-- Bool for EndOfGame is to indicate whether reached 
-- 2048 or not
data Result = EndOfGame State Bool
            | ContinueGame State
        deriving (Eq, Show)



---- For trial
type Player = State -> Char

data Action = Action Char                 
         deriving (Ord,Eq)


--------- For the Update function -----------

data LineState = LineState [Integer] Integer
        deriving (Ord, Eq, Show)


------------------------------------------ GAME ------------------------------------------



type Game = Char -> State -> Result

game2048 :: Game
game2048 dir (State board score) 
    | win updated_board         = EndOfGame (State updated_board new_score) True    
    | isGameOver updated_board  = EndOfGame (State updated_board new_score) False          
    | otherwise                 = ContinueGame (State updated_board new_score)
    where
        tile                        = unsafePerformIO rand
        (State new_board new_score) = (update_val (State (move board dir) score) dir)
        updated_board               = if new_board == board then board else addTile new_board tile


-- WIN
win :: [[Integer]] -> Bool 
win lst =  foldr (\x y -> (foldr (\a b -> if a == 2048 then (True || b) else (False || b)) y x)) False lst


-- Check for gameover
isGameOver :: [[Integer]] -> Bool
isGameOver board = (foldr (\x y -> (isGameOverHorizontal x) && y) True board) && (isGameOverVertical board) && (noZero board)

noZero :: [[Integer]] -> Bool 
noZero lst = foldr (\x y -> (foldr (\a b -> if a == 0 then (False && b) else (True && b)) y x)) True lst


isCombinable:: Integer -> Integer -> Bool
isCombinable x y = (x == y) && (x/=(movable_block)) && (x/=(static_block))


isGameOverHorizontal :: [Integer] -> Bool
isGameOverHorizontal (h1:[]) = True
isGameOverHorizontal (h1:h2:t) 
    | isCombinable h1 h2  =   False
    | otherwise =   True && (isGameOverHorizontal (h2:t))

isGameOverVertical :: [[Integer]] -> Bool
isGameOverVertical ((h1:[]):(h2:[]):(h3:[]):(h4:[]):[]) = not (isCombinable h1 h2) && not (isCombinable h2 h3) && not (isCombinable h3 h4)
isGameOverVertical ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) = not (isCombinable h1 h2) && not (isCombinable h2 h3) && not (isCombinable h3 h4) && (isGameOverVertical (t1:t2:t3:t4:[]))

-- Given the board use the first non-zero element to generate 
-- a random number: 1 or 2
rand :: IO Integer
rand = 
    do
        num <- randomRIO (1, 2 :: Integer)
        return num



numberofZeros :: [[Integer]] -> Integer
numberofZeros lst = foldr (\ i n -> foldr (\ ii nn -> if ii == 0 then nn+1 else nn) n i) 0 lst

addTileRandSpot :: [[Integer]] -> Integer -> IO Integer
addTileRandSpot lst n = 
    do
        num <- randomRIO (1, n :: Integer)
        return num



addTileRand :: [[Integer]] -> Integer -> Integer -> [[Integer]]
addTileRand [[]] n val = [[]]

addTileRand lst 0 val = lst

addTileRand ([]:(h2:t2):t) pos val = []:lst
     where 
        lst = addTileRand ((h2:t2):t) pos val

addTileRand ((h1:t1):t) pos val
    | (h1 == 0) && (pos == 1) = ((val*2):t1):t
    | otherwise = ((h1:tt2):tt)
    where 
        newpos = if (h1 == 0) then (pos - 1) else pos
        (tt2:tt) = addTileRand (t1:t) newpos val
    
addTile lst tile = addTileRand lst n tile
    where n = unsafePerformIO (addTileRandSpot lst ((numberofZeros lst)))



------------------------------ Functions for moving and updating the board ------------------------------

--------------------- MOVE ---------------------

move :: [[Integer]] -> Char -> [[Integer]]

-- Move up

move ((h1:[]):(h2:[]):(h3:[]):(h4:[]):[]) 'w' =
    ((hh1:[]):(hh2:[]):(hh3:[]):(hh4:[]):[])
    where 
        lst = [h1,h2,h3,h4]
        (hh1: hh2: hh3: hh4:[]) = challenge_helper move_l lst

move ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) 'w' = 
    ((hh1:tt1):(hh2:tt2):(hh3:tt3):(hh4:tt4):[])
    where 
        lst = [h1,h2,h3,h4]
        (hh1: hh2: hh3: hh4:[]) = challenge_helper move_l lst
        (tt1:tt2:tt3:tt4:[]) = move (t1:t2:t3:t4:[]) 'w'

-- Move down
move ((h1:[]):(h2:[]):(h3:[]):(h4:[]):[]) 's' =
    ((hh1:[]):(hh2:[]):(hh3:[]):(hh4:[]):[])
    where 
        lst = [h1,h2,h3,h4]
        (hh1: hh2: hh3: hh4:[]) = challenge_helper move_r lst

move ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) 's' = 
    ((hh1:tt1):(hh2:tt2):(hh3:tt3):(hh4:tt4):[])
    where 
        lst = [h1,h2,h3,h4]
        (hh1: hh2: hh3: hh4:[]) = challenge_helper move_r lst
        (tt1:tt2:tt3:tt4:[]) = move (t1:t2:t3:t4:[]) 's'

-- Move left
move lst 'a' = foldr (\ x y -> (challenge_helper move_l x):y) [] lst

-- Move right
move lst 'd' = foldr (\ x y -> (challenge_helper move_r x):y) [] lst


-- Helpers for move

-- Left
move_l :: (Foldable t, Eq a, Num a) => t a -> [a]
move_l lst = foldr (\ a b -> if a == 0 then b++[0] else a:b) [] lst



-- Right
move_r :: (Foldable t, Eq a, Num a) => t a -> [a]
move_r lst = foldl (\ a b -> if b == 0 then 0:a else a++[b]) [] lst


------------------------ CHALLENGE HELPER MOVE ------------------------

-- Moves without moving the -2 tile
challenge_helper::  ([Integer]->[Integer]) -> [Integer] -> [Integer]

challenge_helper move_fun [] = []

challenge_helper move_fun (h1:[]) = [h1]

challenge_helper move_fun (h1:h2:[])
    | h1 == static_block = h1 : challenge_helper move_fun (h2:[])
    | h2 == static_block = challenge_helper move_fun (h1:[]) ++ (h2:[])
    |otherwise = move_fun (h1:h2:[])

challenge_helper move_fun (h1:h2:h3:[])
    | h1 == static_block = h1 : challenge_helper move_fun (h2:h3:[]) 
    | h2 == static_block = challenge_helper move_fun (h1:[])  ++ [static_block] ++ challenge_helper move_fun (h3:[])
    | h3 == static_block = challenge_helper move_fun (h1:h2:[])  ++ (h3:[])
    |otherwise = move_fun (h1:h2:h3:[])


challenge_helper move_fun (h1:h2:h3:h4:[])
    | h1 == static_block = h1 : challenge_helper move_fun (h2:h3:h4:[]) 
    | h2 == static_block = challenge_helper move_fun (h1:[])  ++ [static_block] ++ challenge_helper move_fun (h3:h4:[]) 
    | h3 == static_block = challenge_helper move_fun (h1:h2:[])  ++ [static_block] ++ challenge_helper move_fun (h4:[]) 
    | h4 == static_block = challenge_helper move_fun (h1:h2:h3:[])  ++ (h4:[])
    |otherwise = move_fun (h1:h2:h3:h4:[])



------------------------------ UPDATE ------------------------------

update_val :: State -> Char -> State

-- Update for up
update_val (State ((h1:[]):(h2:[]):(h3:[]):(h4:[]):[]) score) 'w' =
    State ((hh1:[]):(hh2:[]):(hh3:[]):(hh4:[]):[]) (sc1 + score) 
    where 
        lst = [h1,h2,h3,h4]
        (LineState (hh1: hh2: hh3: hh4:[]) sc1) = (update_val_l (LineState lst 0))

update_val (State ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) score) 'w' = 
    State ((hh1:tt1):(hh2:tt2):(hh3:tt3):(hh4:tt4):[]) (score + sc1 + sc2)
    where 
        lst = [h1,h2,h3,h4]
        (LineState (hh1: hh2: hh3: hh4:[]) sc1) = (update_val_l (LineState lst 0))
        (State (tt1:tt2:tt3:tt4:[]) sc2) = update_val (State (t1:t2:t3:t4:[]) 0) 'w'


-- Update for down
update_val (State ((h1:[]):(h2:[]):(h3:[]):(h4:[]):[]) score) 's' =
    State ((hh1:[]):(hh2:[]):(hh3:[]):(hh4:[]):[]) (sc1 + score) 
    where 
        lst = [h1,h2,h3,h4]
        (LineState (hh1: hh2: hh3: hh4:[]) sc1) = (update_val_r (LineState lst 0))

update_val (State ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) score) 's' = 
    State ((hh1:tt1):(hh2:tt2):(hh3:tt3):(hh4:tt4):[]) (score + sc1 + sc2)
    where 
        lst = [h1,h2,h3,h4]
        (LineState (hh1: hh2: hh3: hh4:[]) sc1) = (update_val_r (LineState lst 0))
        (State (tt1:tt2:tt3:tt4:[]) sc2) = update_val (State (t1:t2:t3:t4:[]) 0) 's'



-- Update for left
update_val (State [] score) 'a' = (State [] score)
update_val (State (h:t) score) 'a' = State (lst1:lst2) (score + sc1 + sc2)
    where 
        (LineState lst1 sc1) = update_val_l (LineState h 0)
        (State lst2 sc2) = update_val (State t 0) 'a'


-- Update for right
update_val (State [] score) 'd' = (State [] score)
update_val (State (h:t) score) 'd' = State (lst1:lst2) (score + sc1 + sc2)
    where 
        (LineState lst1 sc1) = update_val_r (LineState h 0)
        (State lst2 sc2) = update_val (State t 0) 'd'


-- Helpers for update
-- Left

update_val_l :: LineState -> LineState
update_val_l (LineState [] score) = (LineState [] score)

update_val_l (LineState (h1:[]) score) = (LineState (h1:[]) score)

update_val_l (LineState (h1:h2:t) score)
   | isCombinable h1 h2 = LineState ((h1*2) : lst1) (score + (h1*2) + sc1)
   | otherwise          = LineState (h1 : lst2) (score + sc2)
   where
       (LineState lst1 sc1) = update_val_l (LineState (t++[0]) 0)
       (LineState lst2 sc2) = update_val_l (LineState (h2:t) 0)


-- Right
update_val_r :: LineState -> LineState
update_val_r (LineState (h1:h2:[]) score)
   | isCombinable h1 h2  = LineState (0:(h2*2):[]) (score + (h2*2))
   | otherwise           = LineState (h1:h2:[]) score

update_val_r (LineState (h1:h2:h3:[]) score)
   | isCombinable h2 h3 = LineState (lst1 ++ [h3*2]) (score + sc1 + (h3*2))
   | otherwise          = LineState (lst2 ++ [h3]) (score + sc2)
   where 
       (LineState lst1 sc1) = update_val_r (LineState (0:h1:[]) 0)
       (LineState lst2 sc2) = update_val_r (LineState (h1:h2:[]) 0)

update_val_r (LineState (h1:h2:h3:h4:[]) score)
   | isCombinable h3 h4 = LineState (lst1 ++ [h4*2]) (score + sc1 + (h4*2))
   | otherwise          = LineState (lst2 ++ [h4]) (score + sc2)
   where 
       (LineState lst1 sc1) = update_val_r (LineState (0:h1:h2:[]) 0)
       (LineState lst2 sc2) = update_val_r (LineState (h1:h2:h3:[]) 0)




------------------------------ CHALLENGE UPDATE ------------------------------

update_val_challenge :: [[Integer]] -> Char -> [[Integer]]

-- Update for up
update_val_challenge ((h1:[]):(h2:[]):(h3:[]):(h4:[]):[]) 'w' =
    ((hh1:[]):(hh2:[]):(hh3:[]):(hh4:[]):[])
    where 
        lst = [h1,h2,h3,h4]
        (hh1: hh2: hh3: hh4:[]) = (challenge_helper update_val_challenge_l lst)

update_val_challenge ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) 'w' = 
    ((hh1:tt1):(hh2:tt2):(hh3:tt3):(hh4:tt4):[])
    where 
        lst = [h1,h2,h3,h4]
        (hh1: hh2: hh3: hh4:[]) = (challenge_helper update_val_challenge_l lst)
        (tt1:tt2:tt3:tt4:[]) = update_val_challenge (t1:t2:t3:t4:[])'w'


-- Update for down
update_val_challenge ((h1:[]):(h2:[]):(h3:[]):(h4:[]):[]) 's' =
    ((hh1:[]):(hh2:[]):(hh3:[]):(hh4:[]):[])
    where 
        lst = [h1,h2,h3,h4]
        (hh1: hh2: hh3: hh4:[]) = (challenge_helper update_val_challenge_r lst)

update_val_challenge ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[])'s' = 
    ((hh1:tt1):(hh2:tt2):(hh3:tt3):(hh4:tt4):[])
    where 
        lst = [h1,h2,h3,h4]
        (hh1: hh2: hh3: hh4:[]) = (challenge_helper update_val_challenge_r lst)
        (tt1:tt2:tt3:tt4:[]) = update_val_challenge (t1:t2:t3:t4:[]) 's'



-- Update for left
update_val_challenge [] 'a' = []
update_val_challenge (h:t) 'a' = (lst1:lst2)
    where 
        lst1 = challenge_helper update_val_challenge_l h
        lst2 = update_val_challenge t 'a'


-- Update for right
update_val_challenge [] 'd' = []
update_val_challenge (h:t) 'd' = (lst1:lst2)
    where 
        lst1 = challenge_helper update_val_challenge_r h
        lst2 = update_val_challenge t 'd'


-- Helpers for update
-- Left

update_val_challenge_l :: [Integer] -> [Integer]
update_val_challenge_l [] = []

update_val_challenge_l (h1:[]) = (h1:[])

update_val_challenge_l (h1:h2:t)
   | isCombinable h1 h2 = ((h1*2) : lst1) 
   | otherwise          = (h1 : lst2) 
   where
       lst1 = update_val_challenge_l (t++[0])
       lst2 = update_val_challenge_l (h2:t)


-- Right
update_val_challenge_r :: [Integer] -> [Integer]
update_val_challenge_r (h1:h2:[])
   | isCombinable h1 h2  = (0:(h2*2):[]) 
   | otherwise           = (h1:h2:[]) 

update_val_challenge_r  (h1:h2:h3:[])
   | isCombinable h2 h3 = (lst1 ++ [h3*2]) 
   | otherwise          = (lst2 ++ [h3])
   where 
       lst1 = update_val_challenge_r (0:h1:[])
       lst2 = update_val_challenge_r (h1:h2:[])

update_val_challenge_r (h1:h2:h3:h4:[]) 
   | isCombinable h3 h4 = (lst1 ++ [h4*2]) 
   | otherwise          = (lst2 ++ [h4]) 
   where 
       lst1 = update_val_challenge_r (0:h1:h2:[]) 
       lst2 = update_val_challenge_r (h1:h2:h3:[])

-- Challenge Game:
-- Counts down your moves and you have to have the tiles combine
-- to the numbers it gives you, doesn't have score
-- Have the given numbers as an array and remove the element each time 
-- it is created. If the array is empty and moves /= -1 then win 
-- if moves == 0 and the array is not empty lose


-- Integer:                Moves
-- [(Integer, Integer)]:   Tiles, first integer is the value of the tile 
--                         second integer is the number of tiles you should
--                         have with this number
-- [Integer]:              Half of the values of the normal tiles to be initialized with
-- Integer:                Number of movable tiles to be initialized with
-- Integer:                Number of static tiles to be initiazlied with

data ChallengeGameEnv = ChallengeGameEnv Integer [(Integer, Integer)] [Integer] Integer Integer
    deriving (Ord, Eq, Show)
challange_games :: [ChallengeGameEnv]
challange_games = [
    (ChallengeGameEnv 10 [(8,2)] [1,1] 0 0),
    (ChallengeGameEnv 15 [(8,3),(16,1)] [1,1] 0 0),
    (ChallengeGameEnv 27 [(8,7),(16,3),(32,1)] [1,1] 0 0),
    (ChallengeGameEnv 60 [(16,8),(32,3),(64,1)] [1,1] 0 0),
    (ChallengeGameEnv 105 [(32,7),(64,3),(128,1)] [1,1] 0 0),
    (ChallengeGameEnv 145 [(64,5),(128,2),(256,1)] [1,1] 0 0),
    (ChallengeGameEnv 185 [(64,6),(128,3),(256,1)] [1,1] 0 0),
    (ChallengeGameEnv 235 [(64,8),(128,4),(256,1)] [1,1] 0 0), 
    (ChallengeGameEnv 265 [(128,4),(256,2),(512,1)] [1,1] 0 0),
    (ChallengeGameEnv 300 [(128,5),(256,2),(512,1)] [1,1] 0 0),
    (ChallengeGameEnv 350 [(128,6),(256,2),(512,1)] [1,1] 0 0),
    (ChallengeGameEnv 405 [(64,14),(256,2),(512,1)] [1,1] 0 0),
    (ChallengeGameEnv 415 [(128,7),(256,2),(512,1)] [1,1] 0 0),
    (ChallengeGameEnv 440 [(64,15),(256,2),(512,1)] [1,1] 0 0),
    (ChallengeGameEnv 465 [(128,8),(256,2),(512,1)] [1,1] 0 0),
    (ChallengeGameEnv 500 [(256,4),(512,2),(1024,1)] [1,1] 0 0),
    (ChallengeGameEnv 265 [(128,4),(256,2),(512,1)] [1,1,2,2,2,2,4,8] 1 0),
    (ChallengeGameEnv 320 [(64,11),(256,2),(512,1)] [1,1,1,2,2,2,8,8] 1 0),
    (ChallengeGameEnv 350 [(128,6),(256,2),(512,1)] [1,1,1,1,1,2,2,8] 1 0),
    (ChallengeGameEnv 400 [(64,14),(256,2),(512,1)] [1,1,2,2,4,4,4,4] 1 0),
    (ChallengeGameEnv 420 [(128,7),(256,2),(512,1)] [1,2,4,4,8,8,8,8] 1 0),
    (ChallengeGameEnv 440 [(64,15),(256,2),(512,1)] [1,2,2,2,2,4,4,8] 1 0),
    (ChallengeGameEnv 465 [(128,8),(256,2),(512,1)] [1,1,1,2,4,4,4,4] 1 0),
    (ChallengeGameEnv 500 [(256,4),(512,2),(1024,1)] [1,1,1,1,2,2,4,8] 1 0),
    (ChallengeGameEnv 500 [(256,4),(512,2),(1024,1)] [1,1,1,1,2,2,4,8] 1 0),
    (ChallengeGameEnv 60 [(32,4),(64,2),(128,1)] [1,1,1,2,4,4,8,8] 2 0),
    (ChallengeGameEnv 105 [(32,7),(64,3),(128,1)] [1,1,1,2,2,2,4,8] 2 0),
    (ChallengeGameEnv 145 [(64,5),(128,2),(256,1)] [1,1,1,2,4,4,8,8] 2 0),
    (ChallengeGameEnv 185 [(64,7),(128,3),(256,1)] [1,1,1,2,2,4,4,8] 2 0), --37th Level in the actual game
    (ChallengeGameEnv 10 [(8,2)] [1,1] 1 2), --49th level in the actual game
    (ChallengeGameEnv 15 [(8,3),(16,1)] [1,1] 1 2),
    (ChallengeGameEnv 27 [(8,7),(16,3),(32,1)] [1,2] 1 2)
    ]

-- [[Integer]]:            board
-- Integer:                moves
-- [(Integer, Integer)]:   tiles, first integer is the value of the tile 
--                         second integer is the number of tiles you should
--                         have with this number
-- Integer:                Index of the current game that's being played

data ChallengeState = ChallengeState [[Integer]] Integer [(Integer, Integer)] Integer
    deriving (Ord, Eq, Show)

data ChallengeResult = EndOfGameChallenge ChallengeState Bool
                    | ContinueGameChallenge ChallengeState 
    deriving (Eq, Show)



type ChallengeGame = Char -> ChallengeState -> ChallengeResult

game2048challenge :: ChallengeGame
game2048challenge dir (ChallengeState board moves tiles curGame) 
    | win_challenge update_board tiles                  = EndOfGameChallenge (ChallengeState final_board update_moves tiles curGame) True   -- agent wins
    | (update_moves == 0) || (isGameOver final_board)   = EndOfGameChallenge (ChallengeState final_board (-1) tiles curGame) False           -- no more moves
    | otherwise                                         = ContinueGameChallenge (ChallengeState final_board update_moves tiles curGame)
    where
        tile            = unsafePerformIO rand
        temp_board      = (move board dir)
        update_board    = update_val_challenge temp_board dir
        final_board     = if update_board == board 
                            then board 
                            else addTile update_board tile
        update_moves    = if update_board == board
                            then moves 
                            else moves - 1


win_challenge:: [[Integer]] -> [(Integer, Integer)] -> Bool
win_challenge board tiles = foldr (\ x y -> (atleast_includes board x) && y) True tiles

        


total_includes:: [[Integer]] -> (Integer, Integer) -> Integer
total_includes board (val, n) = foldr (\ lst num -> (foldr (\ v curnum -> if v > 0 then curnum + (div v val) else curnum) num lst)) 0 board
atleast_includes :: [[Integer]] -> (Integer, Integer) -> Bool
atleast_includes board (val, n) = n <= (total_includes board (val, n))

made_tiles :: [[Integer]] -> [(Integer, Integer)] -> [(Integer, Integer)]
made_tiles board tiles = foldr (\ (p1,p2) y -> if (p2 <= total_includes board (p1,p2)) then (p1,p2):y else (p1, (total_includes board (p1,p2))) :y ) [] tiles