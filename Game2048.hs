module Game2048 where

b  :: [[Integer]]
b = [[2,0,0,0],[0,0,0,2],[0,2,0,0],[2,0,0,0]]

------------ Definitions -------------

data State = State [[Integer]] Integer
        deriving (Ord, Eq, Show)


data Result = EndOfGame State
            | ContinueGame State
        deriving (Eq, Show)


--------- For the Update function -----------

data LineState = LineState [Integer] Integer
        deriving (Ord, Eq, Show)


------------------------------------------ GAME ------------------------------------------

type Game = Char -> State -> Result

game2048 :: Game
game2048 dir (State board score) 
    | win updated_board         = EndOfGame (State board score)   -- agent wins
    | isGameOver updated_board  = EndOfGame (State b 0)           -- no more moves
    | otherwise                 = ContinueGame (State updated_board new_score)
    where
        (State new_board new_score) = (update_val (State (move board dir) score) dir)
        updated_board               = addTile new_board (rand new_board)

-- WIN
win :: [[Integer]] -> Bool 
win lst =  foldr (\x y -> (foldr (\a b -> if a == 2048 then (True || b) else (False || b)) y x)) False lst


-- Check for gameover
isGameOver :: [[Integer]] -> Bool
isGameOver board = (foldr (\x y -> (isGameOverHorizontal x) && y) True board) && (isGameOverVertical board) && (noZero board)

noZero :: [[Integer]] -> Bool 
noZero lst = foldr (\x y -> (foldr (\a b -> if a == 0 then (False && b) else (True && b)) y x)) True lst

isGameOverHorizontal :: Eq a => [a] -> Bool
isGameOverHorizontal (h1:[]) = True
isGameOverHorizontal (h1:h2:t) 
    | h1 == h2  =   False
    | otherwise =   True && (isGameOverHorizontal (h2:t))

isGameOverVertical :: Eq a => [[a]] -> Bool
isGameOverVertical ((h1:[]):(h2:[]):(h3:[]):(h4:[]):[]) = (h1 /= h2) && (h2 /= h3) && (h3 /= h4)
isGameOverVertical ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) = (h1 /= h2) && (h2 /= h3) && (h3 /= h4) && (isGameOverVertical (t1:t2:t3:t4:[]))

-- Given the board use the first non-zero element to generate 
-- a random number: 1 or 2
rand :: [[Integer]] -> Integer
rand lst = if mod_n == 0 then 1 else mod_n
    where 
        n     = rand_choose lst
        mod_n = mod n 3

rand_choose :: [[Integer]] -> Integer
rand_choose [[]] = 0
rand_choose ([]:(h2:t2):t) = rand_choose ((h2:t2):t)
rand_choose ((h1:t1):t)
    | h1 /= 0   = h1
    | otherwise = rand_choose (t1:t)


-- Basic addTile where it adds the tile at the first 0

addTile :: [[Integer]] -> Integer -> [[Integer]]
addTile [[]] n = [[]]

addTile ([]:(h2:t2):t) n = []:lst
     where 
        lst = addTile ((h2:t2):t) n

addTile ((h1:t1):t) n
    | h1 == 0 = ((n*2):t1):t
    | otherwise = ((h1:tt2):tt)
    where 
        (tt2:tt) = addTile (t1:t) n




------------------------------ Functions for moving and updating the board ------------------------------

--------------------- MOVE ---------------------

move :: (Eq a, Num a) => [[a]] -> Char -> [[a]]

-- Move up

move ((h1:[]):(h2:[]):(h3:[]):(h4:[]):[]) 'w' =
    ((hh1:[]):(hh2:[]):(hh3:[]):(hh4:[]):[])
    where 
        lst = [h1,h2,h3,h4]
        (hh1: hh2: hh3: hh4:[]) = move_l lst

move ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) 'w' = 
    ((hh1:tt1):(hh2:tt2):(hh3:tt3):(hh4:tt4):[])
    where 
        lst = [h1,h2,h3,h4]
        (hh1: hh2: hh3: hh4:[]) = move_l lst
        (tt1:tt2:tt3:tt4:[]) = move (t1:t2:t3:t4:[]) 'w'

-- Move down
move ((h1:[]):(h2:[]):(h3:[]):(h4:[]):[]) 's' =
    ((hh1:[]):(hh2:[]):(hh3:[]):(hh4:[]):[])
    where 
        lst = [h1,h2,h3,h4]
        (hh1: hh2: hh3: hh4:[]) = move_r lst

move ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) 's' = 
    ((hh1:tt1):(hh2:tt2):(hh3:tt3):(hh4:tt4):[])
    where 
        lst = [h1,h2,h3,h4]
        (hh1: hh2: hh3: hh4:[]) = move_r lst
        (tt1:tt2:tt3:tt4:[]) = move (t1:t2:t3:t4:[]) 's'

-- Move left
move lst 'a' = foldr (\ x y -> (move_l x):y) [] lst

-- Move right
move lst 'd' = foldr (\ x y -> (move_r x):y) [] lst


-- Helpers for move

-- Left
move_l :: (Foldable t, Eq a, Num a) => t a -> [a]
move_l lst = foldr (\ a b -> if a == 0 then b++[0] else a:b) [] lst

-- Right
move_r :: (Foldable t, Eq a, Num a) => t a -> [a]
move_r lst = foldl (\ a b -> if b == 0 then 0:a else a++[b]) [] lst


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
   | h1 == h2  = LineState ((h1*2) : lst1) (score + (h1*2) + sc1)
   | otherwise = LineState (h1 : lst2) (score + sc2)
   where
       (LineState lst1 sc1) = update_val_l (LineState (t++[0]) 0)
       (LineState lst2 sc2) = update_val_l (LineState (h2:t) 0)


-- Right
update_val_r :: LineState -> LineState
update_val_r (LineState (h1:h2:[]) score)
   | h1 == h2  = LineState (0:(h2*2):[]) (score + (h2*2))
   | otherwise = LineState (h1:h2:[]) score

update_val_r (LineState (h1:h2:h3:[]) score)
   | h2 == h3  = LineState (lst1 ++ [h3*2]) (score + sc1 + (h3*2))
   | otherwise = LineState (lst2 ++ [h3]) (score + sc2)
   where 
       (LineState lst1 sc1) = update_val_r (LineState (0:h1:[]) 0)
       (LineState lst2 sc2) = update_val_r (LineState (h1:h2:[]) 0)

update_val_r (LineState (h1:h2:h3:h4:[]) score)
   | h3 == h4  = LineState (lst1 ++ [h4*2]) (score + sc1 + (h4*2))
   | otherwise = LineState (lst2 ++ [h4]) (score + sc2)
   where 
       (LineState lst1 sc1) = update_val_r (LineState (0:h1:h2:[]) 0)
       (LineState lst2 sc2) = update_val_r (LineState (h1:h2:h3:[]) 0)