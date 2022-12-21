-- pack 5 ' maybe, recursion, structs '

-- task 1 ' quadratic equation '
-- Solve quadratic equation
-- In case it has no roots, return Nothing
quadraticSolver :: Double -> Double -> Double -> Maybe (Double, Double)
quadraticSolver a b c   | diskriminat a b c  >= 0 = Just $ roots a b (diskriminat a b c)
                        | otherwise = Nothing
    where diskriminat a b c = (b^2 - (4 * a * c))
          roots a b d = ((-b - d**0.5) / (2*a),((-b + d**0.5) / (2*a)))
-- D>0: quadraticSolver 3 7 (-6) - Just (-3.0,0.6666666666666666)
-- D=0: quadraticSolver 4 4 1    -  Just (-0.5,-0.5)
-- D<0: quadraticSolver 2 1 1    -  Nothing

-- task 2 ' maybe lists stdlib '
-- Implement the following lists functions using Maybe data structure

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x
-- maybeHead []
-- maybeHead [1, 2, 3]
-- maybeHead "abcde"

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (_:xs) = Just xs
-- maybeTail []
-- maybeTail [1, 2, 3]
-- maybeTail "abcde"

maybeInit :: [a] -> Maybe [a]
maybeInit [] = Nothing
maybeInit [x] = Nothing
maybeInit (x:xs) = Just $ x: help_init xs
        where help_init [x] = []
              help_init (x:xs) = (x: help_init xs)
-- maybeInit []
--  maybeInit [1]
--  maybeInit [1, 2, 3]

-- finds first element x in lst, such that (predicate x == True)
-- if no such element exists, returns Nothing
maybeFind :: (a -> Bool) -> [a] -> Maybe a
maybeFind _ [] = Nothing
maybeFind predicate (x:xs)  | predicate x == True = Just x
                            | otherwise = maybeFind predicate xs
--  maybeFind (\x -> x>10) [1, 20, 10, 15, 30]
--  maybeFind (\x -> x>10) [1, 2, 10, 15, 2]
-- maybeFind (\x -> x>10) [1, 2, 9]
-- maybeFind (\x -> x>10) []


-- task 3 ' pattern matching with data structures '
-- implement undefined functions

data DogBreed = GoldenRetrievers
              | BostonTerriers
              | LabradorRetrievers
              | Poodles
              | BorderCollie
              | Beagle
              | IrishSetter
              | Staffordshire
              | Bull
              | Terrier
    deriving (Show, Eq)

data Gender = Male | Female
    deriving (Show, Eq)

data Dog = Dog { name :: String
               , age :: Int
               , gender :: Gender
               , breed :: DogBreed
               , isGoodBoy :: Bool -- holds for female dogs as well
               } deriving (Show, Eq)

dogs = [ Dog "Leander" 12 Male Beagle False
       , Dog "Ouranos" 1 Male Poodles True
       , Dog "Pegasus" 2 Female Beagle False
       , Dog "Atlas" 8 Female GoldenRetrievers True
       , Dog "Castor" 6 Male LabradorRetrievers True
       , Dog "Apollo" 3 Female Beagle False
       , Dog "Narkissos" 15 Male Beagle True
       , Dog "Dardanos" 7 Female Terrier True
       , Dog "Ajax" 4 Male IrishSetter False
       , Dog "Pyrrhos" 2 Female BorderCollie False
       , Dog "Patroclus" 6 Male Bull True
       , Dog "Iacchus" 4 Female Beagle True ]

-- dogs which are good boys
goodBoys :: [Dog]
goodBoys = [cur_dog | cur_dog<-dogs, isGoodBoy cur_dog ]

-- dogs with name longer than 7 symbols
longNamedDogs :: [Dog]
longNamedDogs = [cur_dog | cur_dog<-dogs, length (name cur_dog) > 7 ]

-- among dogs, which is the most popular gender?
mostPopularDogGender :: Gender
mostPopularDogGender = if fst(counter 0 0 dogs) > snd((counter 0 0 dogs)) then Male else Female
            where   counter male female [] = (male, female)
                    counter male female (x:xs) | gender x == Male = counter (male+1) female xs
                                               | gender x == Female = counter male (female+1) xs 
   

oldestDog :: Dog
oldestDog = help_searcher (age (head dogs)) (head dogs) (tail dogs)
                    where   help_searcher _ old_dog [] = old_dog
                            help_searcher age_old old_dog (x:xs) | age x > age_old = help_searcher (age x) x xs
                                                                 | otherwise = help_searcher age_old old_dog xs 

averageDogAge :: Double
averageDogAge = (fromIntegral (fst counter)) / (fromIntegral (snd counter))
            where counter = foldl (\(sum_age, cn) cur_dog -> (sum_age + (age cur_dog), cn+1) ) (0, 0) dogs

-- finds dogs with given breed
dogsByBreed :: DogBreed -> [Dog]
dogsByBreed cur_breed = [cur_dog | cur_dog <- dogs, (breed cur_dog == cur_breed)]
-- dogsByBreed GoldenRetrievers
-- dogsByBreed Beagle

----- data structures
-- task 4.1

-- Создайте тип, который реализует комплексные числа
-- создайте функции, которые реализуют:
-- - сумму, разницу
-- - умножение, деление
-- - взятие сопряженного
-- - взятие абсолютного значения


data Complex  = Complex { deistv :: Double -- действительная часть
                        , mnimay :: Double -- мнимая часть
                        } deriving (Show, Eq)

examples = [  Complex 12 0
            , Complex (-1) (-3)
            , Complex 3 2
            , Complex 17 (-35)
            , Complex 15 5
            ]
a = Complex 3 2
b = Complex 15 5
c = Complex 1 (-5)
d = Complex 5 2
e = Complex (-2) 1
f = Complex 1 (-1)
z = Complex 4 (-3)

complexSum :: Complex -> Complex -> Complex
complexSum (Complex x_d x_m) (Complex y_d y_m) = Complex (x_d + y_d) (x_m + y_m)
-- complexSum a b

complexSub :: Complex -> Complex -> Complex
complexSub (Complex x_d x_m) (Complex y_d y_m) = Complex (x_d - y_d) (x_m - y_m)
-- complexSub a b

complexMult :: Complex -> Complex -> Complex
complexMult (Complex x_d x_m) (Complex y_d y_m) = Complex (x_d*y_d - x_m*y_m) (x_d*y_m + y_d*x_m)
-- complexMult c d

complexDiv :: Complex -> Complex -> Complex
complexDiv (Complex x_d x_m) (Complex y_d y_m) = Complex  (( x_d*y_d + x_m*y_m ) / (y_d^2 + y_m^2))  (( x_m*y_d - x_d*y_m ) / (y_d^2 + y_m^2))
-- complexDiv e f

complexSoprej :: Complex -> Complex
complexSoprej (Complex x_d x_m) = Complex (x_d) (x_m * (-1))
-- complexSoprej a

complexABS :: Complex -> Double
complexABS (Complex x_d x_m) = (x_d^2 + x_m^2) ** (0.5)


-- Создайте тип, который образует односвязный список (<=> список имеет голову и хвост, либо является пустым)
-- реализуйте  для него следующие методы:

data MyList a = EmptyList | MyList a (MyList a)
    deriving (Show)

--  MyList 2 $ MyList 3 $ MyList 4 EmptyList    - example
--------------------------------------------------------------
fromList :: [a] -> MyList a
fromList [] = EmptyList
fromList (x:xs) = MyList x $ fromList xs

ist = fromList [1, 2, 3] -- example
--------------------------------------------------------------

toList :: MyList a -> [a]
toList EmptyList = []
toList (MyList x (xs)) = [x] ++ (toList xs)

new_ist = toList (MyList 2 $ MyList 3 $ MyList 4 EmptyList)  -- example
---------------------------------------------------------------

reverseMyList :: MyList a -> MyList a
reverseMyList = help_reverse EmptyList
    where   help_reverse acc EmptyList = acc
            help_reverse acc (MyList x (xs)) = help_reverse (MyList x $ acc) xs
-- examples for tests
-- reverseMyList ist
-- toList(reverseMyList ist)
-- toList(reverseMyList (fromList [1]))
-- toList(reverseMyList EmptyList)
-- toList(reverseMyList (fromList [1, 2, 3]))

-----------------------------------------------------------------

-- should do the same thing as standard map
mapMyList :: (a -> b) -> MyList a -> MyList b
mapMyList _ EmptyList = EmptyList
mapMyList f (MyList x (xs)) = MyList (f x) $ mapMyList f xs
-- examples for tests
-- mapMyList (\x -> x+1) EmptyList
-- mapMyList (\x -> x*x) (MyList 1 $ MyList 2 $ MyList 3 EmptyList)
-- mapMyList (\x -> x+1) (MyList 2 EmptyList)
