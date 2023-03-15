updElmBy :: [a] -> Int -> a -> [a]
updElmBy [] _ _ = error "out of range"
updElmBy (x:xs) 0 el = (el:xs)
updElmBy (x:xs) ind el  | ind<0 = error "index must be positive"
                        | otherwise = x: updElmBy xs (ind-1) el


prettyBoard :: (Int, Int) -> (Int, Int) -> String
prettyBoard (x1, y1) (x2, y2) = prettyBoard' 0 (x1, y1) (x2, y2)
    where prettyBoard' 8 _ _ = ""
          prettyBoard' index (x1, y1) (x2, y2)  | (y1 == index) = (updElmBy "_ _ _ _ _ _ _ _\n" (x1*2) 'Q') ++ prettyBoard' (index+1) (x1, y1) (x2, y2) 
                                                | (y2 == index) = (updElmBy "_ _ _ _ _ _ _ _\n" (x2*2) 'q') ++ prettyBoard' (index+1) (x1, y1) (x2, y2) 
                                                | otherwise = "_ _ _ _ _ _ _ _\n" ++ prettyBoard' (index+1) (x1, y1) (x2, y2) 
-- prettyBoard (1, 1) (1, 5)
desk1 = putStr $ prettyBoard (1, 1) (1, 5)
desk2 = putStr $ prettyBoard (2, 2) (1, 5)
desk3 = putStr $ prettyBoard (2, 2) (5, 5)


canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2) = (x1 == x2) || (y1==y2) || (x1 - y1 == x2 - y2) || (x1 + y1 == x2 + y2)

-- canAttack (1, 1) (1, 5) -> True == desk1
-- canAttack (2, 2) (1, 5) -> False   ==  desk2
-- canAttack (2, 2) (5, 5) -> True   ==  desk3
