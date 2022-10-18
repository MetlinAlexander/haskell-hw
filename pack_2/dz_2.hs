-----------------------------
head' :: [Int] -> Int
head' [] = error "empty list"
head' (x:_) = x
-----------------------------
-----------------------------
tail' :: [Int] -> [Int]
tail' [] = error "empty list"
tail' (_:xs) = xs
-----------------------------
-----------------------------
last' :: [Int] -> Int
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs
-----------------------------
-----------------------------
init' :: [Int] -> [Int]
init' [] = error "empty list"
init' [x] = []
init' (x:xs) = (x : init' xs)
-----------------------------
-----------------------------
length' :: [Int] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs
-----------------------------
-----------------------------
null' :: [Int] -> Bool
null' [] = True
null' _ = False
-----------------------------
-----------------------------
drop' :: Int -> [Int] -> [Int]
drop' _ [] = []
drop' n (x:xs)  | (n<=0)==True = (x:xs)
                | otherwise = drop' (n-1) xs
-----------------------------
-----------------------------
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs
-----------------------------
-----------------------------
product' :: [Int] -> Int
product'[] = 1
product' (x:xs) = x * product' xs
-----------------------------
-----------------------------
elem' :: Int -> [Int] -> Bool
elem' _ [] = False
elem' n (x:xs)  
            | n==x = True
            | otherwise = elem' n xs
-----------------------------
-----------------------------
reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
-----------------------------