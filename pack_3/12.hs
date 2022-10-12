----- list comperh----

---1---
fizzbuzz = [if x `mod` 15==0 
                then "Fizzbuzz" 
            else if x `mod` 3==0 
                then "Fizz" 
                else if x `mod` 5==0 
                    then "Buzz" 
                    else show x | x <- [1..]]

---2---
--dotsInCircle :: (Double, Double) -> Double -> [(Double,Double)] -> [(Double, Double)]
--dotsInCircle xs r ys = 

---3---
setAnd :: [Int] -> [Int] -> [Int]
setAnd xs ys = [x | x<-xs,y<-ys, x==y]

----recursion---

---1---
sumdigit :: Int -> Int
sumdigit 0 = 0
sumdigit x = (x `mod` 10)  + sumdigit (x `div` 10)
---2---
cndigit :: Int -> Int
cndigit x =  counter 1 x
    where counter ds n = if n>=10 then counter (ds+1) (n `div` 10) else ds
---3---
pow :: Int -> Bool
pow x = if (x `mod` 2 ==0 && x/=0) then pow (x `div` 2) else if (x==1) then True else False
---4---
sequenceByPred :: (a->a) -> a -> [a]
sequenceByPred f start = start : sequenceByPred f pred
                        where pred = f start
---5---


---6---
--a--
colatz_step :: Int -> Int
colatz_step x = cn 0 x
    where 
        cn st n = if (n `mod` 2==0) then cn (st+1) (n `div` 2) else if (n==1) then st else cn (st+1) (n*3 +1)

--b--
colatz_max :: Int -> Int
colatz_max x = mx 0 x
    where 
        mx m n =if (n `mod` 2==0) then mx m (n `div` 2) else if (n==1) then m else mx (max m (n*3 +1)) (n*3 +1)

---7---
log_dv :: Int -> Int
log_dv x   | x<=0 = error "don,t exist"
           | otherwise = score 1 0 x
           where score acc k x = if (acc>=x) then k else score (acc*2) (k+1) x

---8---
my_sort :: [Int] -> [Int]
my_sort [] = []
my_sort (x:xs) = my_sort [less | less <- xs, less <= x] ++ [x] ++ my_sort [bigger | bigger <- xs, bigger > x]