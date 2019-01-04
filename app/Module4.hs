module Module4 where

import           Data.List

-- Типы перечислений
--
-- типы данных можно делать инстансами классов типов, то есть реализовывать необзодимый интерфейс классов типов
-- Однако, можно делать автомаетическую реализацию представителей - deriving, и компилятор автоматически сможет выводить
-- представителя соответствующего класса типов
data B = F | T deriving (Show, Eq, Read, Enum, Ord)

not' :: B -> B
not' T = F
not' F = T

--
-- тогда мы можем проделать следующее:
-- not' T <- Show
-- T == F <- Eq
-- read "T" :: B <- Read
-- succ F <- Enum
-- F < T <- Ord
--
-- Тип данных Color определен следующим образом
--
data Color = Red | Green | Blue deriving (Read)

-- Определите экземпляр класса Show для типа Color, сопоставляющий каждому из трех цветов его текстовое представление.
instance Show Color where
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue"

--
-- Определите частичную (определенную на значениях от '0' до '9') функцию charToInt.
charToInt :: Char -> Int
charToInt x | x `elem` ['0' .. '9'] = fromEnum x - 48

-- тест: unfoldr (\x -> if x /= ':' then Just (charToInt x, succ x) else Nothing) '0' == [0..9]
--
-- Определите (частичную) функцию stringToColor,
-- которая по строковому представлению цвета как в прошлой задаче возвращает исходный цвет.
stringToColor :: String -> Color
stringToColor x | x `elem` ["Red", "Green", "Blue"] = read x

--
--
-- Тип LogLevel описывает различные уровни логирования.
data LogLevel = Error | Warning | Info

--
-- Определите функцию cmp, сравнивающую элементы типа LogLevel так, чтобы было верно, что Error > Warning > Info.
cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error     = EQ
cmp Error _         = GT
cmp Warning Warning = EQ
cmp Warning Error   = LT
cmp Warning Info    = GT
cmp Info Info       = EQ
cmp Info _          = LT

--
-- в хаскеле можно осуществлять сопоставление с образцом и в правой части функции, т.е. теле функции
-- с помощью конструкции case of
lessThanError :: LogLevel -> Bool
lessThanError lvl =
  case cmp lvl Error of
    LT -> True
    _  -> False

--
-- https://stepik.org/lesson/4916/step/13?unit=1082
-- Пусть объявлен следующий тип данных:
--
--data Result = Fail | Success
--
-- И допустим определен некоторый тип данных SomeData и некоторая функция
--doSomeWork :: SomeData -> (Result,Int)
-- возвращающая результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха.
-- Определите функцию processData, которая вызывает doSomeWork и возвращает строку "Success" в случае ее успешного завершения,
-- либо строку "Fail: N" в случае неудачи, где N — код ошибки.
--
-- Решение:
--
--processData :: SomeData -> String
--processData x =
--  case doSomeWork x of
--    (_, 0) -> "Success"
--    (_, y) -> "Fail: " ++ show y
--
--
-- Типы произведений и сумм произведений
-- Тип произведения - это тип, имеющий такой конструктор данных, который принимает аргумент
--
-- попробуем определить свой тип данных:
-- data Point =
--   Pt Double
--      Double
--   deriving (Show)
-- итак, мы имеемтип данных под названием Point, который имеет конструктор данных Pt, принимающий 2 аргумента
-- использование конструктора: Pt 3.0 4.0
--  -> Pt 3.0 4.0
-- origin :: Point
-- origin = Pt 0.0 0.0
-- также, мы можем получить свойства объекта в сопоставлении с образцом
-- в примере ниже мы используем конструктор данных прямо в сопоставлении с образцом, и поэтому мы можем использовать
-- аргументы конструктора
-- distanceToOrigin :: Point -> Double
-- distanceToOrigin (Pt x y) = sqrt (x ^ 2 + y ^ 2)
-- использование:
-- distanceToOrigin (Pt 3.0 4.0)
--  -> 5.0
--
-- в отличие от обычного использования объекта:
-- idPoint :: Point -> Point
-- idPoint x = x

--
-- https://stepik.org/lesson/4985/step/3?unit=1083
-- Реализуйте функцию distance, возвращающую расстояние между двумя точками.
data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x y) (Point x' y') = sqrt ((x' - x) ^ 2 + (y' - y) ^ 2)

--
-- Итак, имея типы перечислений и типы произведений, можно объединить эти идеи и получить тип сумм произведений
--
-- например, Maybe является типом сумм произвдений
-- data Maybe a = Nothing | Just a
-- в случае конструктора данных Just, он принимает аргумент
-- или можно использовать конструктор данных Nothing, который не принимает аргументов
--

--
-- https://stepik.org/lesson/4985/step/5?unit=1083
-- Определим тип фигур Shape:
--
-- data Shape = Circle Double | Rectangle Double Double
-- У него два конструктора: Circle r — окружность радиуса r, и Rectangle a b — прямоугольник с размерами сторон a и b.
-- Реализуйте функцию area, возвращающую площадь фигуры. Константа pi уже определена в стандартной библиотеке.
data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area s =
  case s of
    (Circle r)      -> pi * r ^ 2
    (Rectangle a b) -> a * b

--
-- https://stepik.org/lesson/4985/step/6?unit=1083
-- В одном из прошлых заданий мы встречали тип Result и функцию doSomeWork:
--
-- data Result = Fail | Success
--
-- doSomeWork :: SomeData -> (Result,Int)
--
-- Функция doSomeWork возвращала результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха.
-- Такое определение функции не является наилучшим, так как в случае успеха мы вынуждены возвращать некоторое значение,
-- которое не несет никакой смысловой нагрузки.
--
-- Используя функцию doSomeWork, определите функцию doSomeWork' так, чтобы она возвращала код ошибки только в случае неудачи.
-- Для этого необходимо определить тип Result'. Кроме того, определите instance Show для Result' так, чтобы show возвращал
-- "Success" в случае успеха и "Fail: N" в случае неудачи, где N — код ошибки.
--
-- Решение:
-- data Result' = Err Int | None
--
-- instance Show Result' where
--     show None = "Success"
--     show (Err y) = "Fail: " ++ show y
--
-- doSomeWork' :: SomeData -> Result'
-- doSomeWork' s =
--   case doSomeWork s of
--     (_, 0) -> None
--     (_, y) -> Err y

--
-- Мы можем определить функцию, которая будет конструировать объекты
-- например:
-- square :: Double -> Shape
-- square a = Rectangle a a
-- таким образом, мы можем скрыть детали реализации типа данных, а предоставить пользователю API для работы с объектами этого типа
-- например, из стандартной библиотеки:
-- 2 Data.Ratio.% 3
-- :i Data.Ratio.%
-- (GHC.Real.%) :: Integral a => a -> a -> GHC.Real.Ratio a
-- оператор % позволяет построить рациональное число, не используя напрямую конструктор этого типа Integral

--
-- https://stepik.org/lesson/4985/step/8?unit=1083
-- Реализуйте функцию isSquare, проверяющую является ли фигура квадратом.

--data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare s =
  case s of
    (Rectangle x y) -> x == y
    _               -> False

-- в case .. of можно использовать только конструкторы данных, но не функции, конструкирующие объекты

--
-- https://stepik.org/lesson/4985/step/9?unit=1083
-- Целое число можно представить как список битов со знаком.
--
-- Реализуйте функции сложения и умножения для таких целых чисел, считая, что младшие биты идут в начале списка,
-- а старшие — в конце.
-- Можно считать, что на вход не будут подаваться числа с ведущими нулями.
data Bit = Zero | One deriving (Show,Eq)
data Sign = Minus | Plus deriving (Show,Eq)
data Z = Z Sign [Bit] deriving (Show,Eq)

add :: Z -> Z -> Z
add (Z x y) (Z x' y') = buildBinNum $ evalSign x * convertNum y + evalSign x' * convertNum y'

mul :: Z -> Z -> Z
mul (Z x y) (Z x' y') = buildBinNum $ evalSign x * evalSign x' * convertNum y * convertNum y'

convertNum :: [Bit] -> Int
convertNum [] = 0
convertNum xs = evalNumHelper xs 0
  where
    evalNumHelper [] _ = 0
    evalNumHelper (x:xs) c =
      case x of
      Zero -> evalNumHelper xs (c+1)
      One  -> 2^c + evalNumHelper xs (c+1)


evalSign :: Sign -> Int
evalSign Plus  = 1
evalSign Minus = -1

buildBinNum :: Int -> Z
buildBinNum x | x < 0 = Z Minus (toBin (x*(-1)))
              | otherwise = Z Plus (toBin x)
  where
    toBin 0 = []
    toBin x | x `mod` 2 == 1 = One : toBin (x `div` 2)
            | x `mod` 2 == 0 = Zero : toBin (x `div` 2)

--
-- Существуют так называмые неопровержимые образцы, для которых не требуется проводить pattern matching, данные сразу
-- привязываются к переменной
-- Это: просто переменная без конструктора, символ подчеркивания
-- Но существуют еще также ленивые образцы
-- Сопоставление с таким образцом всегда завершается удачей, в независимости от данных
-- сопоставление ленивых образцов происходит не в момент привязывания, а в момент использования данных в правой части
-- таким образом, мы заставляем компилятор поверить, что там лежат именно такие данные
-- однако, если данные не удовлетворяют образцу, и если они будут использованы в правой части, произойдет runtime error
-- например:
fromMaybe' ~(Just x) = x
fromMaybe' Nothing = error "!!!"
-- fromMaybe' (Nothing)
--  -> *** Exception: Irrefutable pattern failed for pattern Just x
-- второе выражение недостижимо никогда в такой реализации

