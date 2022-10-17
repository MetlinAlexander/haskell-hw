---------------functions from task-----------
sg' x | x == 0 = 1
      | x > 0 = 0
      | otherwise = error "Arg must be positive!"

inc x | x >= 0 = x + 1
      | otherwise = error "Arg must be positive!"

dec x | x > 0 = x - 1
      | x == 0 = 0
      | otherwise = error "Arg must be positive!"
--------------plus, minus, mlt-------------
--1--
pls:: Integral a => a -> a -> a
pls a 0 = a
pls 0 b = b
pls a b = pls (inc a) (dec b)

mns:: Integral a => a -> a -> a
mns a 0 = a
mns 0 b = 0
mns a b = mns (dec a) (dec b)

mlt:: Integral a => a -> a -> a
mlt _ 0 = 0
mlt 0 _ = 0
mlt a 1 = a
mlt a b = a + mlt a (dec b)

--------------min, max-------------
--2--
max' :: Int -> Int -> Int
max' a b | (a<0 ) || (b<0) = error "Arg must be positive!"
         | (a `mns` b)== 0 = b
         | otherwise = a

min' :: Int -> Int -> Int
min' a b | (a<0 ) || (b<0) = error "Arg must be positive!"
         | (a `mns` b)== 0 = a
         | otherwise = b

--------------Quotient, remainder, divisibility----------
--3--
div_rec:: Integral a => a -> a -> a
div_rec 0 _ = 0
div_rec _ 0 = 0
div_rec a b | (a<0 ) || (b<0) = error "Arg must be positive!"
               | otherwise =  rec_help a b a
                where rec_help a b acc | (b `mlt` acc<=a) = acc
                                       | otherwise =  rec_help a b (dec acc)
--4--
div_co_rec:: Integral a => a -> a -> a
div_co_rec 0 _ = 0
div_co_rec _ 0 = 0
div_co_rec a b | (a<0 ) || (b<0) = error "Arg must be positive!"
            | otherwise =  rec_help a b 1
            where rec_help a b acc   | (b `mlt` acc>a) = (dec acc)
                                     | (b `mlt` acc==a) = acc
                                     | otherwise =  rec_help a b (inc acc)
--5--
mod_co_rec:: Integral a => a -> a -> a
mod_co_rec 0 _ = 0
mod_co_rec _ 0 = 0
mod_co_rec a b | (a<0 ) || (b<0) = error "Arg must be positive!"
            | otherwise =  rec_help a b 0
            where rec_help a b acc   | (b `mlt` (div_rec a b) `pls` acc)==a  = acc
                                     | otherwise =  rec_help a b (inc acc)

mod_rec:: Integral a => a -> a -> a
mod_rec 0 _ = 0
mod_rec _ 0 = 0
mod_rec a b | (a<0 ) || (b<0) = error "Arg must be positive!"
               | otherwise =  rec_help a b b
                where rec_help a b acc | (b `mlt` (div_rec a b) `pls` acc)==a  = acc
                                       | otherwise =  rec_help a b (dec acc)
--6--
predicat 0 _ = True
predicat _ 0 = False
predicat a b = (a `mod_rec` b) == 0

-------------more tasks with arifmetics----------
--7--
nd:: Int->Int
nd 0 = error "Infinite"
nd 1 = 1
nd x = rec_help x 1 1
      where rec_help x acc cn | (acc>=(x `div_rec` 2)+1) = cn + 1
                              | x `predicat` acc = rec_help x (acc+1) (cn+1)
                              | otherwise = rec_help x (acc+1) cn
--8--
sumd :: Int->Int
sumd 0 = error "Infinite"
sumd 1 = 1
sumd x = rec_help x 2 (x+1)
      where rec_help x acc sum| (acc>=(x `div_rec` 2)+1) = sum
                              | (x `predicat` acc) = rec_help x (acc+1) (sum+acc)
                              | otherwise = rec_help x (acc+1) sum

--9--
prim :: Int -> Bool
prim 0 = error "nothing"
prim 1 = False
prim x = rec_help x 2
            where rec_help x acc | (acc>=(x `div_rec` 2)+1) = True
                                 | (x `predicat` acc) = False
                                 | otherwise = rec_help x (acc+1)

--10--
pnd:: Int-> Int
pnd 0 = error "Infinite"
pnd x = rec_help x 1 0
      where rec_help x acc cn | (x<0) = error "Arg must be positive!"
                              |(acc>x) = cn
                              | (prim acc)==False = rec_help x (acc+1) cn
                              | x `predicat` acc = rec_help x (acc+1) (cn+1)
                              | otherwise = rec_help x (acc+1) cn
--11--
nod:: Int->Int->Int
nod 0 0 = 0
nod 0 _ = error "Infinite"
nod _ 0 = error "Infinite"
nod a b = rec_help a b 1 1
            where rec_help a b acc nd | (a<0 ) || (b<0) = error "Arg must be positive!"
                                      | (a<acc ) || (b<acc) = nd
                                      | (a `predicat` acc) && (b `predicat` acc) = rec_help a b (acc+1) acc
                                      | otherwise = rec_help a b (acc+1) nd

--12--
nok:: Int->Int->Int
nok 0 _ = error "Infinite"
nok _ 0 = error "Infinite"
nok a b = rec_help a b 1
      where rec_help a b acc  | (a<0 ) || (b<0) = error "Arg must be positive!"
                              --  | (a<acc ) || (b<acc) = nd
                              | (acc `predicat` a) && (acc `predicat` b) = acc
                              | otherwise = rec_help a b (acc+1)
----------Minimization operator----------
--13--
predel = 1000

minimiz g = minimiz' g 0
      where minimiz' g y x | (y == predel) = Nothing
                           | (g y x == 0) = Just y
                           | otherwise = minimiz' g (y+1) x

minimizTwoArg g = minimiz' g 0
      where minimiz' g y x1 x2 | (y == predel) = Nothing
                               | (g y x1 x2 == 0) = Just y
                               | otherwise = minimiz' g (y+1) x1 x2

--examples :
-- 1) (\t x -> x - t) 123 -> 123
-- 2) (\t x -> x*x + t) 12 -> Nothing
-- 3) (\t x -> x*x - t) 12 -> 144
--14--
pred_minim_bool g = minimiz' g 0
      where minimiz' g y x | (y == predel) = Nothing
                           | (g y x == True) = Just y
                           | otherwise = minimiz' g (y+1) x

pred_minim_Int g = minimiz' g 0
      where minimiz' g y x | (y == predel) = False
                           | (g y x == 0) = True
                           | otherwise = minimiz' g (y+1) x

pred_minim_Two_arg_bool g = minimiz' g 0
      where minimiz' g y x1 x2 | (y == predel) = Nothing
                               | (g y x1 x2 == True) = Just y
                               | otherwise = minimiz' g (y+1) x1 x2

pred_minim_Two_arg_int g = minimiz' g 0
      where minimiz' g y x1 x2 | (y == predel) = False
                               | (g y x1 x2 == 0) = True
                               | otherwise = minimiz' g (y+1) x1 x2

--15--
sqrt' x = pred_minim_bool (\t x -> x < (t+1)*(t+1)) x
--16--
divs x1 x2 = pred_minim_Two_arg_bool (\t x1 x2 -> x1<((t+1)*x2)) x1 x2