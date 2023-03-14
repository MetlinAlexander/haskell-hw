import Barans

fromJust :: Maybe a -> a
fromJust (Just t) = t

-- 1. Найти дедушку -- отца матери
grandfathers :: Sheep -> Maybe Sheep
grandfathers cur_sheep = do
    cur_mother <- mother cur_sheep
    cur_ded <- father cur_mother
    return cur_ded

-- grandfathers "i12" -> Just "i9"
-- grandfathers "i1" -> Nothing

-- 2. Найти прадеда (отца найденного выше)

grandgrandfather :: Sheep -> Maybe Sheep
grandgrandfather cur_sheep = do
    cur_ded <- grandfathers cur_sheep
    cur_praded <- father cur_ded
    return cur_praded 
-- grandgrandfather "i12" -> Just "i5"
-- grandgrandfather "i8" -> Nothing

-- 3. Найти список всех родителей и список бабушек с дедушками для данного барашка (т.е. две функции)

-- all_parents :: Sheep -> [Maybe Sheep]
-- all_parents cur_sheep = [father cur_sheep, mother cur_sheep]
all_parents :: Sheep -> Maybe [Sheep]
all_parents cur_sheep = do
    cur_mother <- mother cur_sheep
    cur_father <- father cur_sheep
    return [cur_father, cur_mother]

-- all_parents "i1" -> Nothing
-- all_parents "i12" -> Just ["i6","i11"]

all_grandparents :: Sheep -> Maybe [[Sheep]]
all_grandparents cur_sheep = do
    parents <- all_parents cur_sheep
    mother_parents <- all_parents (last parents)
    father_parents <- all_parents (head parents)
    return [mother_parents, father_parents]

-- all_grandparents "i12" -> Just [["i7","i3"],["i5","i3"]]
-- all_grandparents "i13" -> Nothing

-- 4. Является ли он сиротой (по базе данных)?
-- No parents -> Sirota

sirota :: Sheep -> Bool
sirota cur_sheep    | (all_parents cur_sheep) == Nothing = True
                    | otherwise = False
-- sirota "i12" -> False

-- sirota "i1" -> True

-- Пусть есть список селекционных барашков:

selected_barans = ["i3", "i5", "i6", "i9", "i12"]

-- 5. Нужно написать функцию, которая для данного барашка возвращает имя селекционного папы, 
-- обёрнутого конструктором Just, и Nothing, если такового нет (например Just "i6").

sheep_in_selcted :: Sheep -> Maybe Sheep
sheep_in_selcted cur_sheep = helper selected_barans cur_sheep
        where helper [] _ = Nothing
              helper (x:xs) cur_sheep | x == cur_sheep = Just cur_sheep
                                      | otherwise = helper xs cur_sheep

selcted_father :: Sheep -> Maybe Sheep
selcted_father cur_sheep = do
    cur_father <- father cur_sheep
    in_select_father <- sheep_in_selcted cur_father
    return in_select_father
-- selcted_father "i12" -> Just "i6"
-- selcted_father "i8" -> Nothing

-- 6. Нужно написать функцию, 
-- которая находит селекционного ближайшего предка по мужской линии 
-- (либо, если такового нет, возвращает Nothing).

nearest_man :: Sheep -> Maybe Sheep
nearest_man cur_sheep   | (father cur_sheep) == Nothing = Nothing
                        | (selcted_father cur_sheep) == Nothing = nearest_man $ fromJust $ (father cur_sheep)
                        | otherwise = selcted_father cur_sheep
-- nearest_man "i12" -> Just "i6"