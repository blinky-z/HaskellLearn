module Module4 where

import Data.List

-- Типы перечислений
--
-- типы данных можно делать инстансами классов типов, то есть реализовывать необзодимый интерфейс классов типов
-- Однако, можно делать автомаетическую реализацию представителей - deriving, и компилятор автоматически сможет выводить
-- представителя соответствующего класса типов
data B
  = F
  | T
  deriving (Show, Eq, Read, Enum, Ord)

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
data Color
  = Red
  | Green
  | Blue
  deriving (Read)

-- Определите экземпляр класса Show для типа Color, сопоставляющий каждому из трех цветов его текстовое представление.
instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

--
-- Определите частичную (определенную на значениях от '0' до '9') функцию charToInt.
charToInt :: Char -> Int
charToInt x
  | x `elem` ['0' .. '9'] = fromEnum x - 48

-- тест: unfoldr (\x -> if x /= ':' then Just (charToInt x, succ x) else Nothing) '0' == [0..9]
--
-- Определите (частичную) функцию stringToColor,
-- которая по строковому представлению цвета как в прошлой задаче возвращает исходный цвет.
stringToColor :: String -> Color
stringToColor x
  | x `elem` ["Red", "Green", "Blue"] = read x

--
--
-- Тип LogLevel описывает различные уровни логирования.
data LogLevel
  = Error
  | Warning
  | Info

--
-- Определите функцию cmp, сравнивающую элементы типа LogLevel так, чтобы было верно, что Error > Warning > Info.
cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Error _ = GT
cmp Warning Warning = EQ
cmp Warning Error = LT
cmp Warning Info = GT
cmp Info Info = EQ
cmp Info _ = LT

--
-- в хаскеле можно осуществлять сопоставление с образцом и в правой части функции, т.е. теле функции
-- с помощью конструкции case of
lessThanError :: LogLevel -> Bool
lessThanError lvl =
  case cmp lvl Error of
    LT -> True
    _ -> False

--
-- Пусть объявлен следующий тип данных:
--
-- data Result = Fail | Success
--
-- И допустим определен некоторый тип данных SomeData и некоторая функция
-- doSomeWork :: SomeData -> (Result,Int)
-- возвращающая результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха.
-- Определите функцию processData, которая вызывает doSomeWork и возвращает строку "Success" в случае ее успешного завершения,
-- либо строку "Fail: N" в случае неудачи, где N — код ошибки.
--
-- Решение:
--
-- processData :: SomeData -> String
-- processData x =
--   case doSomeWork x of
--     (_, 0) -> "Success"
--     (_, y) -> "Fail: " ++ show y
