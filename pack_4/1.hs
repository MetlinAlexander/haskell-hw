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
decem_to_bin :: Int -> Int
decem_to_bin = (num_from_list . list_bin)
      where list_bin 0 = []
            list_bin x = list_bin (x `div` 2) ++ [x `mod` 2]
            ---
            num_from_list xs = helper xs 0
            ----
            helper [] acc = acc
            helper (x:xs) acc = helper xs (acc*10 + x)
--2
from_n_to_decem :: Int -> Int -> Int -- work with n<=10
from_n_to_decem n x = fst (foldl (\(acc, pow) x -> ((acc + x*(n^pow)), pow+1)) (0, 0) (list_num x))
            where list_num 0 = []
                  list_num x = [x `mod` 10] ++ list_num (x `div` 10)

--3
num_from_string :: String -> Int
num_from_string x = foldl  (\acc y -> read y + acc) 0 [x]
--4
search_n :: [Int] -> Int
search_n x = (((count x) * ((count x) + 1)) `div` 2) - summ x
      where summ x  = foldl (\acc x -> acc+x) 0 x
            count x = foldl (\acc x -> acc+1) 1 x
