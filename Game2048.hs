module Game2048 where

board :: [[Integer]]
board = [[0,0,2,0],[0,2,0,2],[0,2,4,0],[2,0,2,0]]




-- MOVE:
move :: (Eq a, Num a) => [[a]] -> Char -> [[a]]

-- Move up
move ((h1:t1):(h2:t2):[]) 'w'
    | h1 == 0   = (h2:t1):(0:t2):[]
    | otherwise = (h1:t1):(h2:t2):[]

move ((h1:t1):(h2:t2):(h3:t3):[]) 'w'
    | h1 == 0   = move ((h2:t1):(h3:t2):[]) 'w' ++ (0:t3):[]
    | otherwise = (h1:t1): move ((h2:t2):(h3:t3):[]) 'w'

move ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) 'w'
    | h1 == 0   =  move ((h2:t1):(h3:t2):(h4:t3):[]) 'w' ++ (0:t4):[]
    | otherwise = (h1:t1): move ((h2:t2):(h3:t3):(h4:t4):[]) 'w'

-- Move down
move ((h1:t1):(h2:t2):[]) 's'
    | h2 == 0   = (0:t1):(h1:t2):[]
    | otherwise = (h1:t1):(h2:t2):[]

move ((h1:t1):(h2:t2):(h3:t3):[]) 's'
    | h3 == 0   = (0:t1) : move ((h1:t2):(h2:t3):[]) 's'
    | otherwise = move ((h1:t1):(h2:t2):[]) 's' ++ (h3:t3):[]

move ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) 's'
    | h4 == 0   = (0:t1) : move ((h1:t2):(h2:t3):(h3:t4):[]) 's'
    | otherwise = move ((h1:t1):(h2:t2):(h3:t3):[]) 's' ++ (h4:t4):[]

-- Move left
move lst 'a' = foldr (\ x y -> (foldr (\ a b -> if a == 0 then b++[0] else a:b) [] x):y) [] lst

-- Move right
move lst 'd' = foldr (\ x y -> (foldl (\ a b -> if b == 0 then 0:a else a++[b]) [] x):y) [] lst


-- UPDATE VALUE:
update_val :: (Eq a, Num a) => [[a]] -> Char -> [[a]]

-- Update for up
update_val ((h1:t1):(h2:t2):[]) 'w'
    | h1 == h2     = ((h1*2) : t1): ((0:t2) : [])
    | otherwise    = (h1:t1) : ((h2:t2) : [])

update_val ((h1:t1):(h2:t2):(h3:t3):[]) 'w'
    | h1 == h2     = ((h1*2) : t1): update_val ((h3:t2) : (0:t3) : []) 'w'
    | otherwise    = (h1:t1) : update_val ((h2:t2) : (h3:t3) : []) 'w'

update_val ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) 'w'
    | h1 == h2     = ((h1*2) : t1): update_val ((h3:t2) : (h4:t3) : (0:t4):[]) 'w'
    | otherwise    = (h1:t1) : update_val ((h2:t2) : (h3:t3) : (h4:t4):[]) 'w'


-- Update for down
update_val ((h1:t1):(h2:t2):[]) 's'
    | h1 == h2     = (0:t1):((h2*2):t2):[]
    | otherwise    = (h1:t1):(h2:t2):[]

update_val ((h1:t1):(h2:t2):(h3:t3):[]) 's'
    | h2 == h3     = (update_val ((0:t1):(h1:t2):[]) 's') ++ ((h3*2):t3):[]
    | otherwise    = (update_val ((h1:t1):(h2:t2):[]) 's') ++ (h3:t2):[]

update_val ((h1:t1):(h2:t2):(h3:t3):(h4:t4):[]) 's'
    | h3 == h4     = (update_val ((0:t1):(h1:t2):(h2:t3):[]) 's') ++ ((h4*2):t4):[]
    | otherwise    = (update_val ((h1:t1):(h2:t2):(h3:t3):[]) 's') ++ (h4:t4):[]


-- Update for left
update_val lst 'a' = foldr (\ x l -> (update_val_l x):l) [] lst


-- Update for right
update_val lst 'd' = foldr (\ x l -> (update_val_r x):l) [] lst


-- Helpers for right and left
-- Left
update_val_l :: (Eq a, Num a) => [a] -> [a]

update_val_l [] = []

update_val_l (h1:[]) = [h1]

update_val_l (h1:h2:t)
   | h1 == h2  = (h1*2) : update_val_l (t++[0])
   | otherwise = h1 : update_val_l (h2:t)

-- Right
update_val_r :: (Eq a, Num a) => [a] -> [a]

update_val_r (h1:h2:[])
   | h1 == h2  = 0:(h2*2):[]
   | otherwise = h1:h2:[]

update_val_r (h1:h2:h3:[])
   | h2 == h3  = update_val_r (0:h1:[]) ++ [h3*2]
   | otherwise = update_val_r (h1:h2:[]) ++ [h3]

update_val_r (h1:h2:h3:h4:[])
   | h3 == h4  = update_val_r (0:h1:h2:[]) ++ [h4*2]
   | otherwise = update_val_r (h1:h2:h3:[]) ++ [h4]