module Module_5_1_tasks where

import           Data.Char    (toUpper)
import           Data.Functor

-- Задачи

--
-- -----------------------
-- 5.1 Класс типов Functor и законы для него
-- -----------------------
--

--
-- https://stepik.org/lesson/8432/step/3?unit=2743
-- Определите представителя класса Functor для следующего типа данных, представляющего точку в трёхмерном пространстве:
data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

--
-- https://stepik.org/lesson/8432/step/4?unit=2743
-- Определите представителя класса Functor для типа данных GeomPrimitive, который определён следующим образом:
data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a) deriving Show
-- При определении, воспользуйтесь тем, что Point3D уже является представителем класса Functor.
instance Functor GeomPrimitive where
    fmap f (Point p)           = Point (fmap f p)
    fmap f (LineSegment p1 p2) = LineSegment (fmap f p1) (fmap f p2)

-- почему это работает и почему это не входит в рекурсию?
-- дело в том, что как раз так и для этого и нужна абстракция
-- ведь fmap вызывается разный, в зависимости от типа упаковочного контейнера, и мы можем работать с контейнерами на основе этого
-- а так как Point3D уже является представителем класса Functor, то мы можем вызывать fmap на нем
-- если бы Point3D не был представителем класса Functor, мы бы не смогли вызывать fmap на основе типа контейнера:
-- • No instance for (Functor Point3D) arising from a use of ‘fmap’

--
-- https://stepik.org/lesson/8432/step/6?unit=2743
-- Определите представителя класса Functor для бинарного дерева, в каждом узле которого хранятся элементы типа Maybe:
data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

-- можно вместо функции fmap применять оператор <$>, который делает то же самое что и fmap
-- тогда можно писать так:

instance Functor Tree where
    fmap f (Leaf x)       = Leaf (f <$> x)
    fmap f (Branch l x r) = Branch (f <$> l) (f <$> x) (f <$> r)

--
-- https://stepik.org/lesson/8432/step/8?unit=2743
-- Определите представителя класса Functor для типов данных Entry и Map.
-- Тип Map представляет словарь, ключами которого являются пары:
data Entry k1 k2 v = Entry (k1, k2) v deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v] deriving Show
-- В результате должно обеспечиваться следующее поведение:
-- fmap применяет функцию к значениям в словаре, не изменяя при этом ключи.
instance Functor (Entry k1 k2) where
--    fmap (a -> b) -> (Entry k1 k2) a -> (Entry k1 k2) b
  fmap f (Entry (k1, k2) v) = Entry (k1, k2) (f v)

instance Functor (Map k1 k2) where
--    fmap (a -> b) -> (Map k1 k2) a -> (Map k1 k2) b
  fmap f (Map xs) = Map (map (fmap f) xs)

-- этот вариант просто для большего понимания, что тут происходит:
-- так как у нас Entry уже является представителем тайпкласса Functor, то все что нам надо сделать,
-- это применить fmap на каждом элементе нашей мапы
-- и fmap будет вызываться нужный: по конструктору типа Entry k1 k2, принимающему один параметр
--
--
--instance Functor (Map k1 k2) where
--    fmap (a -> b) -> (Map k1 k2) a -> (Map k1 k2) b
--    fmap f (Map xs) = Map (fmapHelper f xs)
--      where
--        fmapHelper :: (a -> b) -> [Entry k1 k2 a] -> [Entry k1 k2 b]
--        fmapHelper f []     = []
--        fmapHelper f (x:xs) = fmap f x : fmapHelper f xs
