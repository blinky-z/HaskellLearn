module Module4 where

import           Data.Char     (isDigit, isSpace)
import           Data.Function
import           Data.List     (isInfixOf)
import           Data.Maybe

-- -----------------------
-- 4.1 Типы перечислений
-- -----------------------
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
-- -----------------------
-- 4.2 Типы произведений и сумм произведений
-- -----------------------
--
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
-- таким образом, мы можем скрыть детали реализации типа данных, а предоставить пользователю API для работы с
-- объектами этого типа
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
--
--fromMaybe' ~(Just x) = x
--fromMaybe' Nothing   = error "!!!"
-- fromMaybe' (Nothing)
--  -> *** Exception: Irrefutable pattern failed for pattern Just x
-- второе выражение недостижимо никогда в такой реализации

--
-- -----------------------
-- 4.3 Синтаксис записей
-- -----------------------
--
-- В хаскеле, как и в ООП, можно создавать структуры данных, например:
data Person = Person { firstName :: String, lastName :: String, age :: Int }
  deriving (Show, Eq)
-- тогда можно читать поля структуры так:
john = Person "John" "Smith" 33
-- age john
--  -> 33
-- john
--  -> Person {firstName = "John", lastName = "Smith", age = 33}
-- john&firstName (уже более похоже на обычное ооп, где используется для доступа к полям точка. Необходимо
-- импортировать Data.Function)
--  -> "John"
-- (&) :: a -> (a -> b) -> b
-- x & f = f x
-- как видно, & просто использует функцию firstName на объекте

--
-- https://stepik.org/lesson/5431/step/3?unit=1132
-- Определите тип записи, который хранит элементы лога. Имя конструктора должно совпадать с именем типа, и
-- запись должна содержать три поля:
-- timestamp — время, когда произошло событие (типа UTCTime);
-- logLevel — уровень события (типа LogLevel);
-- message — сообщение об ошибке (типа String).
-- Определите функцию
-- logLevelToString
-- , возвращающую текстуальное представление типа
-- LogLevel
-- , и функцию
-- logEntryToString
-- , возвращающую текстуальное представление записи в виде:
--
-- <время>: <уровень>: <сообщение>
--
--
-- Для преобразование типа UTCTime в строку используйте функцию timeToString.
--
-- Решение:
--timeToString :: UTCTime -> String
--timeToString = formatTime defaultTimeLocale "%a %d %T"
--
--data LogLevel = Error | Warning | Info
--
--data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String}
--
--logLevelToString :: LogLevel -> String
--logLevelToString lvl =
--  case lvl of
--    Error   -> "Error"
--    Warning -> "Warning"
--    Info    -> "Info"
--
--logEntryToString :: LogEntry -> String
--logEntryToString entry = timeToString (timestamp entry) ++ ": " ++ logLevelToString (logLevel entry) ++ ": " ++ message entry

--
-- также можно создавать объекты в таком синтаксисе:
xavier = Person {age = 40, firstName = "Phideaux", lastName = "Xavier"}
-- этот синтаксис позволяет перечислять поля не в строгом порядке, как они идут в конструкторе типа,
-- а также позволяет вообще не указывать некоторые поля, но при использовании данных полей возникнет runtime error

--
-- можно модифицировать поля таким образом:
updateAge newAge person = person {age = newAge}
-- updateAge 42 xavier
--  -> Person {firstName = "Phideaux", lastName = "Xavier", age = 42}

--
-- Определите функцию updateLastName person1 person2, которая меняет фамилию person2 на фамилию person1.
--data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 {lastName = lastName p1}

-- с использованием оператора &
updateLastName' :: Person -> Person -> Person
updateLastName' p1 p2 = p2 {lastName = p1&lastName}

--
-- можно использовать синтаксис запией в pattern matching
-- без pattern matching:
name :: Person -> String
name person = firstName person ++ " " ++ lastName person

-- связывает соответвующие поля по порядку, такое мы уже использовали ранее
name' :: Person -> String
name' (Person fn ln _) = fn ++ " " ++ ln

-- связывает только нужные нам поля, подходит для больших структур
name'' :: Person -> String
name'' Person {lastName = ln, firstName = fn} = fn ++ " " ++ ln

-- сравнение:
isRectangle :: Shape -> Bool
isRectangle Rectangle {} = True
isRectangle _            = False

isRectangle' :: Shape -> Bool
isRectangle' (Rectangle _ _) = True
isRectangle' _               = False

--
-- Определить функцию abbrFirstName, которая сокращает имя до первой буквы с точкой, то есть, если имя было "Ivan",
-- то после применения этой функции оно превратится в "I.". Однако, если имя было короче двух символов, то оно не меняется.
--data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName person@Person {firstName = fn} | length fn > 2 = person {firstName = head fn : ['.']}
                                             | otherwise = person

--
-- -----------------------
-- 4.4 Типы с параметрами
-- -----------------------
--
-- Типы с параметрами - тип, параметризованный другим типом
-- пусть есть 2 типа координат - через точки на ней и разделенные на ячейки
--data CoordD = CoordD Double Double

--data CoordI = CoordI Int Int

-- большинство операций на этих координатах будет совпадать, поэтому нам нужен универсальный тип, который
-- вместо конкретного будет иметь произвольный тип:
--data Coord a = Coord a a
-- использование:
-- Coord (3::Int) (4::Int)
--
-- :t Coord (3::Int) (4::Int)
--  -> Coord (3::Int) (4::Int) :: Coord Int
--
-- конструктор Coord полиморфен:
-- :t Coord
--  -> Coord :: a -> a -> Coord a
-- Конструктор Coord принимает 2 значения произвольных типов (a и a) и конструирует значение типа (Coord a)
-- в приведенном выше примере мы видим различие между конструкторами данных и конструкторами типа
-- конструктор данных порождает выражения
-- конструктор типа же, принимает произвольный тип a и конструирует значение типа (Coord a)
-- Говорится, что конструктор типа Coord параметризован - он содержит некоторую аппликацию,
-- т.е. применяется к какому-то произвольному типу a и производит новый тип
-- когда мы вызываем команду :t Coord - мы на самом деле спрашиваем не конструктор типа, а конструктор данных
-- например, пусть Coord определен так:
--data Coord a = Coord' a a
-- :t Coord
--  -> error: • Data constructor not in scope: Coord
-- :t Coord'
--  -> Coord' :: a -> a -> Coord a

--
-- https://stepik.org/lesson/5746/step/3?unit=1256
-- Реализуйте функции distance, считающую расстояние между двумя точками с вещественными координатами, и manhDistance,
-- считающую манхэттенское расстояние между двумя точками с целочисленными координатами.
data Coord a = Coord a a deriving Show

distance' :: Coord Double -> Coord Double -> Double
distance' (Coord x y) (Coord x' y') = sqrt((x' - x) ^ 2 + (y' - y) ^ 2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x y) (Coord x' y') = abs(x'-x) + abs(y' - y)

--
-- https://stepik.org/lesson/5746/step/4?unit=1256
-- Плоскость разбита на квадратные ячейки. Стороны ячеек параллельны осям координат.
-- Координаты углов ячейки с координатой (0,0) имеют неотрицательные координаты.
-- Один из углов этой ячейки имеет координату (0,0). С ростом координат ячеек увеличиваются координаты точек внутри этих ячеек.
--
-- Реализуйте функции getCenter, которая принимает координату ячейки и возвращает координату ее центра, и функцию getCell,
-- которая принимает координату точки и возвращает номер ячейки в которой находится данная точка.
-- В качестве первого аргумента обе эти функции принимают ширину ячейки.
--data Coord a = Coord a a

getCenter :: Double -> Coord Int -> Coord Double
getCenter s (Coord x y) = Coord (s * fromIntegral (x + 1) - s / 2) (s * fromIntegral (y + 1) - s / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell s (Coord x y) = Coord x' y'
  where
    x' = if x < 0 then truncate (x/s) - 1 else truncate $ x/s
    y' = if y < 0 then truncate (y/s) - 1 else truncate $ y/s


--
-- https://stepik.org/lesson/5746/step/6?unit=1256
-- Реализуйте функцию, которая ищет в строке первое вхождение символа, который является цифрой,
-- и возвращает Nothing, если в строке нет цифр.

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) | isDigit x = Just x
                 | otherwise = findDigit xs

-- попробовал для себя через свертку решить, но это менее эффективно, чем реализация выше, из-за того что приходится
-- разворачивать весь список, прежде чем начать его обрабатывать
findDigit' xs = foldl f Nothing xs
  where
    f xs x | isDigit x =
              case xs of
                Nothing -> Just x
                _       -> xs
           | otherwise = xs
--  здесь pattern matching в левой части уравнения происходит, более красивый код
    f' Nothing x = if isDigit x then Just x else Nothing
    f' xs _      = xs

--
-- https://stepik.org/lesson/5746/step/7?unit=1256
-- Реализуйте функцию findDigitOrX, использующую функцию findDigit (последнюю реализовывать не нужно). findDigitOrX должна находить
-- цифру в строке, а если в строке цифр нет, то она должна возвращать символ 'X'. Используйте конструкцию case.
--findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX xs =
  case findDigit xs of
    Nothing -> 'X'
    Just x  -> x

-- с помощью функции fromMaybe
findDigitOrX' = fromMaybe 'X' . findDigit

--
-- https://stepik.org/lesson/5746/step/8?unit=1256
-- Maybe можно рассматривать как простой контейнер, например, как список длины 0 или 1.
-- Реализовать функции maybeToList и listToMaybe,
-- преобразующие Maybe a в [a] и наоборот (вторая функция отбрасывает все элементы списка, кроме первого).
maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ head xs

--
-- https://stepik.org/lesson/5746/step/9?unit=1256
-- Реализуйте функцию parsePerson, которая разбирает строки вида firstName = John\nlastName = Connor\nage = 30 и
-- возвращает либо результат типа Person, либо ошибку типа Error.
--
-- Строка, которая подается на вход, должна разбивать по символу '\n' на список строк, каждая из которых имеет вид X = Y.
-- Если входная строка не имеет указанный вид, то функция должна возвращать ParsingError.
-- Если указаны не все поля, то возвращается IncompleteDataError.
-- Если в поле age указано не число, то возвращается IncorrectDataError str, где str — содержимое поля age.
-- Если в строке присутствуют лишние поля, то они игнорируются.
data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

--data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson xs = helper (lines xs) "" "" ""
  where
    helper :: [String] -> String -> String -> String -> Either Error Person
    helper [] fn ln age =
      if fn /= "" && ln /= "" && age /= ""
        then
          if filter (not . isDigit) age /= ""
            then Left (IncorrectDataError age)
            else Right (Person fn ln (read age :: Int))
        else Left IncompleteDataError
    helper (currentField:xs) fn ln age =
      case matchCorrectField currentField of
        True
          | trim (takeWhile (/= '=') currentField) == "firstName" -> helper xs (getValue currentField) ln age
          | trim (takeWhile (/= '=') currentField) == "lastName" -> helper xs fn (getValue currentField) age
          | trim (takeWhile (/= '=') currentField) == "age" -> helper xs fn ln (getValue currentField)
          | otherwise -> helper xs fn ln age
        False -> Left ParsingError

    getValue xs = trim $ tail $ dropWhile (/= '=') xs

    trim xs = trimHelper $ trimHelper xs
      where
        trimHelper = reverse . dropWhile isSpace

    matchCorrectField :: String -> Bool
    matchCorrectField xs = '=' `elem` xs

--
-- В хаскеле существует еще одна система типов над уже существующими типами
-- Эта система типов называется kind
-- Эта система типов показывает тип не выражения, а тип типа:
-- :k Char
--  -> * Char :: *
-- :k Int
--  -> Int :: *
-- :k Maybe
--  -> Maybe :: * -> *
-- :k Maybe Int
--  -> Maybe Int :: *
-- данная система типов нужна для контроля за типами
-- например, вызвав :k [],  мы можем узнать, сколько выражений какого-то типа надо передать в конструктор типа,
-- чтобы получилось правильное выражение
-- :k []
-- [] :: * -> *
-- мы видим, что чтобы образовать тип [], надо передать в конструктор типа один тип
-- ведь объявление [] такое:
-- data [] a
-- итак, :t показывает тип выражения или конструктор данных, а :k показывает тип конструктора типа
--
-- еще один пример:
-- мы не можем построить функцию, которая принимает Int, а возвращает [], так как конструктор типа [] параметризован:
-- :k Int -> []
--  -> error:
--    • Expecting one more argument to ‘[]’
--      Expected a type, but ‘[]’ has kind ‘* -> *’
--    • In the type ‘Int -> []’
-- но можем построить такую функцию:
-- :k Int -> [] Int
--  -> Int -> [] Int :: *
-- или такую: :k Int -> [] Char
--  -> Int -> [] Char :: *

--
-- https://stepik.org/lesson/5746/step/12?unit=1256
-- Исправьте ошибку в приведенном коде.
--eitherToMaybe :: Either a -> Maybe a
--eitherToMaybe (Left a) = Just a
--eitherToMaybe (Right _) = Nothing
eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a)  = Just a
eitherToMaybe (Right _) = Nothing

--
-- В хаскеле можно заставить конструктор данных вычислять переданные параметры сразу при создании объекта:
-- data CoordStrict = Coord Strict!a !a
-- если бы не ставили!, то данные не вычислялись бы, пока не были использованы
-- например:
-- data CoordLazy = CoordLazy a a
-- CoordLazy 1 undefined <- работает

--
-- Module4_5