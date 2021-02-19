module Trial2 where

board = [[2,0,0,0],[2,0,0,0],[2,0,0,0],[2,0,0,0]]

--update_val :: (Eq a, Num a) => [a] -> [a]
--update_val [] = []

--update_val (h1:[]) = [h1]

--update_val (h1:h2:t)
--    | h1 == h2  = (h1*2) : update_val (t++[0])
--    | otherwise = h1 : update_val (h2:t)


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
