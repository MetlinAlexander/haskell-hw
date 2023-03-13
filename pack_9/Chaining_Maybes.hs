import Prelude hiding (head, tail, maximum)


type GreekData = [(String, [Integer])]
greekDataA :: GreekData
greekDataA = [ ("alpha", [5, 10])
             , ("beta", [0, 8])
             , ("gamma", [18, 47, 60])
             , ("delta", [42])
             ]

greekDataB :: GreekData
greekDataB = [ ("phi", [53, 13])
             , ("chi", [21, 8, 191])
             , ("psi", [1])
             , ("omega", [6, 82, 144])
             ]

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

divMay :: Double -> Double -> Maybe Double
divMay _ 0 = Nothing
divMay a b = Just $ a / b

-- maximumMay :: [a] -> Maybe a
maximumMay :: Ord a => [a] -> Maybe a
maximumMay []  = Nothing
maximumMay (x:xs) = Just $ maximumMay' x xs
                where maximumMay' x [] = x
                      maximumMay' x (y:ys) = maximumMay' (if y>x then y else x) ys

-- tailMay :: [a] -> Maybe a
tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay [_] = Nothing
tailMay (_:xs) = Just xs

{-
 tl;dr implement the function WITHOUT do-notation or any monad magic. only pattern-matching, where and let in

 first query the GreekData that is passed in,
 look up the string passed in the second argument,
 and retrieve the corresponding list of Integers. Call this list xs.
 Next calculate the maximum of the tail of xs
 (Don’t use any pattern matching here.
 Use case expressions and the maximumMay and tailMay functions)
 Take the maximum and divide it by the head of the list (using headMay and divMay functions).
 If any of these operations along the way return Nothing, then your function should return Nothing.
 But if everything succeeds, then return the final quotient.
 One hint… you’ll need to use the fromIntegral function to convert your two Integers to Doubles for the final call to divMay.
-}

{-
tl; dr реализуйте функцию БЕЗ do-notation или any monad magic. только pattern-matching, where and let in

сначала запросите переданные GreekData,
найдите строку, переданную во втором аргументе,
и извлеките соответствующий list of Integers. Назовем этот список xs.
Затем вычислите maximum of the tail of xs
(здесь не используйте никакого pattern matching.
Используйте case expressions and the maximumMay and tailMay functions)
Возьмите максимум и разделите его на начало списка (using headMay and divMay functions).
Если какая-либо из этих операций на этом пути возвращает Nothing, то ваша функция должна возвращать Nothing-.
Но если все пройдет успешно, то верните окончательное частное.
Одна подсказка ... вам нужно будет использовать функцию fromIntegral, чтобы преобразовать ваши два целых числа в Double для окончательного вызова divMay.
-}
-- import Data.Maybe as mb

fromJust :: Maybe a -> a
fromJust (Just t) = t

queryGreek :: GreekData -> String -> Maybe Double
queryGreek [] str = Nothing
queryGreek ((str1, int_list):xs) str      | (str1 == str) = if ( (maximumMay int_list /= Nothing) && (tailMay int_list /= Nothing)) 
                                                            then divMay (max_in_tail int_list) (head_double int_list)
                                                            else Nothing
                                          | otherwise = queryGreek xs str
                                                where head_double list  = (fromIntegral (fromJust $ headMay list ) :: Double)
                                                      max_in_tail list = (fromIntegral (fromJust $ maximumMay $ fromJust $ tailMay list ) :: Double)

-- queryGreek greekDataA "alpha" == Just 2.0
-- queryGreek greekDataB "psi" == Nothing

to_double_Maybe :: Integer -> Maybe Double
to_double_Maybe a = Just $ (fromIntegral a :: Double) 

-- Now do the same whole thing, but using do-notation, since Maybe is a Monad
queryGreekPro :: GreekData -> String -> Maybe Double
queryGreekPro [] _ = Nothing
queryGreekPro ((str1, int_list):xs) str   | (str == str1) = do
                                                            head' <- headMay int_list
                                                            head_double <- to_double_Maybe head'
                                                            tail' <- tailMay int_list
                                                            max' <- maximumMay tail'
                                                            max_double <- to_double_Maybe max'
                                                            divMay max_double head_double
                                          | otherwise = queryGreekPro xs str

--queryGreekPro greekDataA "alpha" == Just 2.0
-- queryGreekPro greekDataB "psi" == Nothing

-- * a harder task. rewrite queryGreekPro, but without the do-notation, only using the (>>=) operator and its friends
-- in other words, desugarize your notation
queryGreekProPlus :: GreekData -> String -> Maybe Double
queryGreekProPlus = undefined