module Module4_6_task12 where

import           Prelude hiding (fromList, lookup)

--
-- https://stepik.org/lesson/7602/step/12?unit=1473
-- Реализуйте представителя MapLike для типа ArrowMap, определенного ниже.

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap {getArrowMap :: k -> Maybe v}

instance MapLike ArrowMap where
  empty = ArrowMap (const Nothing)
  lookup k (ArrowMap f) = f k
  insert k v (ArrowMap f) = ArrowMap (\x -> if x == k then Just v else f x)
  delete k (ArrowMap f) = ArrowMap (\x -> if x == k then Nothing else f x)
  fromList []         = empty
  fromList ((k,v):xs) = insert k v (fromList xs)

-- здесь лежит одна функция
-- когда мы вставляем новую функцию, мы должны написать другую функцию, которая сохраняет все старые ключи, но для нового ключа
-- будет выдавать новое значение

-- операция delete будет реализована так, что на значении удаляемого ключа k мы просто будем выдавать Nothing, иначе вызывать
-- раннее упакованную функцию

-- как проверять, выдает ли функция на такой ключ что-то или нет
-- можно просто вызвать эту же функцию на переданном ключе, и она вернет или значение по этому ключу, или Nothing гарантированно
-- да, пользовательно может взять и построить сам с помощью конструктора такой контейнер:
-- ArrowMap (\5 -> Just 6)
-- но это его проблемы, обычно конструктор вообще не виден публично, и контейнер строится с помощью fromList,
-- а fromList реализован так, что после добавления всех ключей, в конце будет лежать всегда Nothing

-- тесты:
a = ArrowMap (\5 -> Just 5)
b = insert 4 4 a
--
t1 = getArrowMap b 4 == Just 4
t2 = getArrowMap b 5 == Just 5
c1 = delete 5 b
c2 = delete 5 a
c3 = delete 4 b
t3 = getArrowMap c1 5 == Nothing
t4 = (getArrowMap c1) 4 == Just 4
t5 = (getArrowMap c2) 5 == Nothing
t6 = (getArrowMap c3) 4 == Nothing
t7 = (getArrowMap c3) 5 == Just 5
-- выше совершенно неправильные тесты, я тестил совершенно не то
-- я пытался зачем-то доставать упакованную функцию и применять к ней аргументы, но это уже совсем не мапа получается,
-- моей же задачей было реализовать именно мапу, ассоциативный контейнер, а его значения достаются с помощью lookup,
-- а сам контейнер строится с помощью fromList или же с помощью empty
-- мы не должны сами строить так вообще:
-- ArrowMap (/5 -> Just 6)
--
m1 :: ArrowMap Int String
m1 = (insert 1 "one" empty)
t8 = lookup 1 m1 == Just "one"
t9 = lookup 2 m1 == Nothing
m2 :: ArrowMap Int String
m2 = fromList [(1, "one"), (2, "two"), (3,"three")]
t10 = lookup 1 m2 == Just "one"
t11 = lookup 4 m2 == Nothing
t = t8 && t9 && t10 && t11