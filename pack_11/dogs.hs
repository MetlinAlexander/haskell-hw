import Data.List

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


{-
dogs = [ Dog "Leander" 12 Male Beagle False
       , Dog "Ouranos" 1 Male Poodles True
       , Dog "Pegasus" 2 Female Beagle False
       , Dog "Atlas" 8 Female GoldenRetrievers True
       , Dog "Castor" 6 Male LabradorRetrievers True
       , Dog "Apollo" 3 Female Beagle False
       , Dog "Narkissos" 15 Male Beagle True
       , Dog "Dardanos" 7 Female Terrier True
       , Dog "Ajax" 4 Male IrishSetter False
       , Dog "Pyrrhos" 2 Female BorderCollie FalseE
       , Dog "Patroclus" 6 Male Bull True
       , Dog "Iacchus" 4 Female Beagle True ]
-}

dogsAge :: Int -> [Dog]
dogsAge cur_age = [cur_dog | cur_dog <- dogs, (age cur_dog == cur_age)]

-- examples
-- how many dogs are of age 2, 4 and 6?
-- dogsAge246 :: [[Dog]]
dogsAge246 :: [(Dog, Dog, Dog)]
dogsAge246 = do
       dogsAge2 <- dogsAge 2
       dogsAge4 <- dogsAge 4
       dogsAge6 <- dogsAge 6
       return $ (dogsAge2, dogsAge4, dogsAge6)

extra_dogs = [  Dog "Zyzz" 5 Male Beagle True
              , Dog "Volt" 4 Male Poodles True
              , Dog "Gerda" 5 Female IrishSetter False
              , Dog "Loonie" 4 Female GoldenRetrievers True ]

extended_dogs = dogs ++ extra_dogs

-- using do-notation, find such dogs, that they are male, 4-5 years old, not IrishSetter, good boys
-- and such dogs, that they are female, 4-5 years old, name is longer then 4 symbols
-- after finding those two groups, combine a list of all combinations they could be mated


dogsQuery :: [[(Dog, Dog)]]--[[(Dog, Dog)]]
dogsQuery = do
       -- dog1 <- [cur_dog | cur_dog <- extended_dogs, (gender cur_dog == Male), (age cur_dog == 4 || age cur_dog == 5), (breed cur_dog /= IrishSetter), isGoodBoy cur_dog]
       -- dog2 <- [cur_dog | cur_dog <- extended_dogs, (gender cur_dog == Female), (age cur_dog == 4 || age cur_dog == 5), (length (name cur_dog) > 4)]
       return $ concat $ [ zip cur_list_1 cur_list_2 | cur_list_1 <- dog1, cur_list_2 <- dog2]-- (permutations dog2)
       where dog1 = permutations $ [cur_dog | cur_dog <- extended_dogs, (gender cur_dog == Male), (age cur_dog == 4 || age cur_dog == 5), (breed cur_dog /= IrishSetter), isGoodBoy cur_dog]
             dog2 = permutations $ [cur_dog | cur_dog <- extended_dogs, (gender cur_dog == Female), (age cur_dog == 4 || age cur_dog == 5), (length (name cur_dog) > 4)]

