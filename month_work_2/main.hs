
-- 2.1 Списочные функции из Prelude

import Prelude hiding ((!!),init,reverse,(++),cycle,take,elem)
------1------
(!!) :: [a] -> Int -> a
(!!) [] _    = error "index out of range"
(!!) (x:_) 0 = x
(!!) (_:xs) index | index<0 = error "index must be positive"
                  | otherwise = xs !! (index - 1)

-- > [1,2,3] !! 0
-- > 1
------2------
init :: [a] -> [a]
init []     = error "empty list"
init [x]    = []
init (x:xs) = (x:init xs)

-- > init [1,2,3]
-- > [1,2]

------3------
(++) :: [a] -> [a] -> [a]
(++) [] y     = y
(++) (x:xs) y = (x: xs ++ y)

-- > [1,2,3] ++ [2,3,3,4]
-- [1,2,3,2,3,3,4]
-- []++ [1, 2, 3] 
-- take 5 ([]++ [1..])
-- take 20 ([7, 6, 5, 3, 134]++ [1..])

------4------
cycle :: [a] -> [a]
cycle []  = []
cycle ist = [ curelem| temp<-[1..], curelem<-ist]

-- take 20 $ cycle [1,2,3]
-- cycle []
-- take 20 $ cycle [666]

------5------
take :: Int -> [a] -> [a]
take (0) ist      = []
take _ []         = []
take count (x:xs) = (x: take (count-1) xs)

-- take 5 [1..]
-- [1,2,3,4,5]
------6------
inits :: [a] -> [[a]]
inits []  = [[]]
inits ist = inits (init ist) ++ [ist]

--  inits [1,2,3]
-- [[],[1],[1,2],[1,2,3]]

tails :: [a] -> [[a]]
tails []         = [[]]
tails all@(x:xs) = (all: tails xs)

-- > tails [1,2,3]
-- [[1,2,3],[2,3],[3],[]]

------7------
elem :: Eq a => a -> [a] -> Bool
elem _ []   = False
elem el (x:xs)  | el == x   = True
                | otherwise = elem el xs
--  1 ‘elem‘ [1,2,3]
-- True
-- 1 `elem` [2, 3, 4]
-- 1 `elem` []
------8------
nub :: Eq a => [a] -> [a]
nub lst = helper [] lst
        where helper [] (x:xs) = helper [x] xs
              helper ans [] = ans
              helper ans (x:xs) | (x `elem` ans == False) = helper (ans ++ [x]) xs
                                | otherwise = helper ans xs
-- > nub [1,2,2,2]
-- [1,2]
-- nub [2, 2, 3, 3, 3, 3, 4, 4, 5, 6, 5, 1, 1]
-- [2,3,4,5,6,1]

------9------

updElmBy :: [a] -> Int -> a -> [a]
updElmBy [] _ _ = error "out of range"
updElmBy (x:xs) 0 el = (el:xs)
updElmBy (x:xs) ind el  | ind<0 = error "index must be positive"
                        | otherwise = x: updElmBy xs (ind-1) el

-- Пример использования:
-- lst2 = updElmBy lst 3 ’a’
-- updElmBy [] 3 'a'  -> out of range
-- updElmBy "Yello" 0 'H' -> Hello
-- updElmBy "Hello rorld" 6 'w' -> Hello world
-- updElmBy "Hello rorld" (-1) 'w'
-- updElmBy "Hello rorld" (20) 'w'

------10------
swp :: [a] -> Int -> Int -> [a]
swp [] _ _ = error "Empty list"
swp lst i j | (i<0 || j<0) = error "index must be positive"
            |otherwise = helperSwp lst i j (lst !! i) (lst !! j)
            where helperSwp lst i j temp_i temp_j = updElmBy (updElmBy lst i temp_j) j temp_i
                  
-- swp [4, 2, 3, 1, 5] 0 3 -> [1,2,3,4,5]
-- swp [4, 2, 3, 1, 5] 0 10

------11------
permutations :: [a] -> [[a]]
permutations []  = [[]]
permutations ist = [ (ist !! ind) : tail' | ind<-[0 .. (length ist)-1], tail' <- permutations $ (del ind ist)]
            where del index (x:xs)  | index == 0 =  xs
                                    | otherwise = x : del (index-1) xs 

-- -- > permutations [1,2,3]
-- [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
------12------
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = (subsequences xs) ++ (subs x (subsequences xs))
            where subs _ []     = []
                  subs x (y:ys) = (x:y):(subs x ys) 

-- > subsequences [1,2,2,3]
-- [[],[1],[2],[1,2],[2],[1,2],[2,2],[1,2,2],[3],[1,3],[2,3],[1,2,3],[2,3],[1,2,3],[2,2,3],[1,2,2,3]]

-- 2.2 Задачи со свертками
------13------
cubsum :: [Int] -> Int
cubsum = foldr (\x acc -> acc + x^3) 1
-- cubsum [1, 2, 3] -> 37

------14------
cubsumLeft :: [Int] -> Int
cubsumLeft = foldl (\acc x -> acc + x^3) 1
-- cubsumLeft [1, 2, 3]

------15------
fact :: Int -> Int
fact fct = foldl (\acc x -> acc * x) 1 [1..fct]

expT :: Double -> Int -> Double
expT x n = foldl (\acc arg -> acc + ( (x^arg) / (fromIntegral (fact arg))) ) 0 [0..n]
-- > expT 1 5
-- 2.7166666666666663
-- > expT 1 10
-- 2.7182818011463845
------16------
howmany :: Eq a => a -> [a] -> Int
howmany el = foldl (\acc x -> if x == el then acc + 1 else acc) 0 
-- > howmany 'l' "hello"
-- 2

------17------
good = "aeiou"
bad = "tnrsh"
howmany_g_b_letters :: [Char] -> (Int,Int)
howmany_g_b_letters = foldl (\(g, b) x -> if x `elem` good then (g+1, b) else if x `elem` bad then (g, b+1) else (g, b)) (0, 0)

-- > howmany_g_b_letters "hello"
-- (2,1)
------18------
intersperse :: a -> [a] -> [a]
intersperse el = foldl (\acc x -> if null acc then acc++[x] else acc++[el]++[x]) [] 
-- > intersperse ',' "hello"
-- ”h,e,l,l,o”
-- intersperse ',' ""
-- intersperse 666 [1, 2, 3, 4, 5]
------19------
cycleshift (x:xs) = xs ++ [x]
rotate [] = []
rotate lst = helper lst lst
            where helper [x] lst = [lst]
                  helper (_:xs) lst = helper xs (cycleshift lst) ++ [lst]
                  -- ++ helper xs cycleshift lst
-- > rotate [1..4]
-- [[4,1,2,3],[3,4,1,2],[2,3,4,1],[1,2,3,4]]