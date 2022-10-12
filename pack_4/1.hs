---------------pac_4---------------
-----string-----
--printf(string, [args])
printf :: String -> [Int] -> String
printf "" _ = ""
printf [x] _ = [x]
printf x [] = x
printf (x:xs) args | (x == '%' && xs!!0=='d') = (show (head args)) ++ printf (tail xs) (tail args)
                   | otherwise = [x] ++ printf xs args
-----math-----
--1
list_bin :: Int -> [Int]
list_bin 0 = []
list_bin x = list_bin (x `div` 2) ++ [x `mod` 2]

num_from_list :: [Int] -> Int
num_from_list xs = helper xs 0
    where helper [] acc = acc
          helper (x:xs) acc = helper xs (acc*10 + x)

decem_to_bin :: Int -> Int
decem_to_bin x = (num_from_list . list_bin) x
--2
from_n_to_decem :: Int -> Int -> Int -- work with n<=10
from_n_to_decem n x = helper n x 0
                    where helper _ 0 _ = 0
                          helper n x acc = (x `mod` 10)*(n^acc) + helper n (x `div` 10) (acc+1)
--3
num_from_string :: String -> Int
--num_from_string [] = 0
num_from_string x = read x
--4
search_n :: [Int] -> Int
search_n x = 1 + helper x 0
            where helper [] acc = acc
                  helper (x:xs) acc = helper xs (max x acc) 
-----other-----
check_scob :: String -> Bool
check_scob [] = True