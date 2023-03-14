import Data.Char
-- example:
-- makeArt 0 == ""
-- makeArt 1
-- wil result in:
-- a
-- makeArt 3
{-
**a**
*b*b*
c***c
*b*b*
**a**
-}

updElmBy :: [a] -> Int -> a -> [a]
updElmBy [] _ _ = error "out of range"
updElmBy (x:xs) 0 el = (el:xs)
updElmBy (x:xs) ind el  | ind<0 = error "index must be positive"
                        | otherwise = x: updElmBy xs (ind-1) el

makeArt :: Int -> String
makeArt 0 = ""
makeArt 1 = "a"
makeArt size = unlines $ add_letters (97) (real_size `div` 2) matrix
    where   matrix = stars real_size
            real_size = (size*2-1)
            new_string str index to_ord =  updElmBy (updElmBy str index $ (toEnum to_ord :: Char)) (real_size - index - 1) $ (toEnum to_ord :: Char) 
            add_letters _ _ [] = []
            add_letters cur_ord index (x:xs)    | index >= 0 = [new_string x index cur_ord] ++ add_letters (cur_ord+1) (index-1) xs
                                                | otherwise = inverse_add (cur_ord-2) (index+2) (x:xs)
            inverse_add _ _ [] = []
            inverse_add cur_ord index (x:xs) = [new_string x index cur_ord] ++  inverse_add (cur_ord-1) (index+1) xs

stars :: Int -> [String]
stars size = [take size (cycle "*") | x <- [1..size]]

main = putStr $ makeArt 3