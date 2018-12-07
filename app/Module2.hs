module Module2 where
import Data.Function

-- Полиморфная функция - параметрический полиморфизм
getSecondFrom :: t1 -> t2 -> t3 -> t2
getSecondFrom a b c = b
-- Сколько разных функций (т.е. возвращающих разные значения) можно реализовать с таким объявлением
-- https://stepik.org/lesson/8417/step/4?unit=1555
--a -> a -> b -> a -> a
--Ответ: 3 функции, это :
-- 1) f :: a -> a -> b -> a -> a
-- f a b c d = a
-- 2) f :: a -> a -> b -> a -> a
-- f a b c d = b
-- 3) f :: a -> a -> b -> a -> a
-- f a b c d = d
--
-- Функции высших порядков
-- Передача бинарного оператора (*) и функции h = snd в функцию `on`
-- тип функции on :
-- (b -> b -> c) -> (a -> b) -> a -> a -> c
-- функция on применяет функцию h на каждый переданный аргумент, а потом к вычеслоенным значениям применяет бинарный оператор g

multSecond = g1 `on` h1
g1 = (*)
h1 = (snd)

multSecondRes = multSecond ('A',2) ('E',7)
-- тут самое интересное в том, что multSecond не принимает аргументов!
-- это работает так:
-- так как multSecond эквивалентно выражению (on g h), а функция on ожидает оператор (он передан - g), функцию (также передана - h),
-- то нам остается вызвать функцию multSecond таким образом: multSecond arg1 arg2, и это превращается в: on g h arg1 arg2
-- сначала не понял, а потом как афигел от этого
-- это называется бесточечный стиль
--
-- Реализовать свою функцию on3, которая в качестве первого аргумента принимает не бинарный оператор, а трехместную функцию

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)
--
-- Композиция функций, бесточечный стиль
doItYourself = f . g . h

f = logBase 2

g = (** 3)

h = max 42

--
--Сколько разных всегда завершающихся функций с типом a -> (a,b) -> a -> (b,a,a) можно реализовать?
--https://stepik.org/lesson/12398/step/5?unit=2828
f1 :: t1 -> (t1,t2) -> t1 -> (t2,t1,t1)
f1 a (b,c) d | True = (c,a,b)
             | True = (c,a,d)
             | True = (c,b,a)
             | True = (c,b,d)
             | True = (c,d,a)
             | True = (c,d,b)
             | True = (c,a,a)
             | True = (c,b,b)
             | True = (c,d,d)
--
-- карирование функций
-- есть функция on - она первым аргументом ожидает некарированную функцию, бинарный оператор
-- тип on: (b -> b -> c) -> (a -> b) -> a -> a -> c, как видно в (b -> b -> c) - это некарированная функция
-- тогда мы не можем написать так: fst `on` (^2),
-- так как тип fst такой:
-- (a, b) -> a - ожидается аргумент пара элементов (кортеж), то есть нельзя передать сначала 1 элемент потом 2 элемент пары,
-- кортеж - 2 неотделимых элемента и требуется передавать сразу оба элемента как пару
-- для того что бы это стало возможным надо применить функцию curry:
-- curry fst `on` (^2)
-- тогда тип выражения curry fst `on` (^2)
-- c => c -> c -> c,
-- что удовлетворяет условию первого аргумента функции on
-- как это сработало?
-- дело в том, что функция curry f x y равносильна такому = f (x, y)
-- то есть функция on как бы применяет к выражению curry f два аргумента x y, а вот далее это уже превращается в f (x, y)
-- однако это не важно, функция on смогла применить аргументы к функции curry fst, и это самое главное, так как вместо
-- функции, принимающей кортеж, т.е. fst (x,y) она увидела вот это: curry fst x y, а такая функция некарированная
--
-- итак: curry позволяет использовать некарированную функцию в качестве карированной
-- и наоборот: uncurry позволяет использовать карированную функцию в качестве некарированной
--
-- реализация curry:   curry f x y             =  f (x, y)
-- реализация uncurry: uncurry f p             =  f (fst p) (snd p)
-- Пример использования uncurry - uncurry (flip const) (5,6)
--


-- Заменить выражение curry id на эквивалентное https://stepik.org/lesson/12398/step/7?unit=2828
--(,)
--


-- Заменить выражение uncurry (flip const) на эквивалентное https://stepik.org/lesson/12398/step/8?unit=2828
--snd
--


-- Реализовать функцию swap https://stepik.org/lesson/12398/step/9?unit=2828
-- uncurry (flip (,))

-- Как это работает
-- Смотрим на объявление функции uncurry:
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- Первое, что принимает функция - это карированную функцию, принимающую 2 аргумента и возвращаюущю какой-то тип c
-- тип c - это может быть и a, и b, и (a,b) или (b,a)
-- далее идет кортеж, пара, и на выходе получается значение типа cи, что логично, ведь uncurry должна выдать результат работы
-- карированной функции (a -> b -> c)
-- (a -> b -> c) - наша функция (flip (,))
-- (a, b) - и является переданной в функцию парой, в котором необходимо переставить элементы местами

-- объявления функций flip и (,)
-- flip:
-- flip :: (a -> b -> c) -> b -> a -> c
-- (,):
-- (,) :: a -> b -> (a, b)
-- рассмотрим подробнее выражение flip (,):
-- как видно, первое чего ожидает flip, это карированной функции которая ожидает 2 аргумента типа a и b, и на выходе дает тип c
-- далее flip ждет сами аргументы типа b и a, и на выходе дает тип c, то есть тот же что и принятая функция, что логично, т.к.
-- flip всего лишь применяет к переданной функции типа (a -> b -> c) операнды в обратном порядке
-- Например: flip (++) "hello" "world" - это "worldhello"
-- теперь то понятно, что происходит в выражении flip (,) x y:
-- (,) - это карированная функция построения пары, которая как и требуется в flip принимает 2 аргумента и возвращает тип c
-- этот тип c - это пара
-- однако, как мы заметили ранее, flip меняет эти операнды местами, и тогда результат будет таким - (y,x)
-- однако, это еще не все, так как к вызову swap у нас применяются не аргументы по одному, а передается пара (x,y)
-- то необходимо применить uncurry: uncurry (flip (,))
-- тогда вызов swap с парой (x,y) произойдет так:
-- 1) swap (x, y)
-- 2) uncurry (flip (,)) (x,y)
-- в работу ступает uncurry принимает аргумент функцию f = flip (,) и пару p =x,y)
-- 3) flip (,) (fst p) (snd p)
-- 4) flip (,) x y
-- 5) (,) y x
-- 5) (y,x)
--
-- Классы типов
-- класс типов - это что-то вроде интерфейса
-- тогда мы можем работать с аргументами не как с конкретными типами, а как с типами, на которых поддерживаются какие-то определенные
-- операции
-- Реализовать класс типов Printable, у которого есть один метод toString, который преобразует значение типа Printable в строку
-- и сделать типы Bool и () представителями этого класса типов
-- это значит, что мы добавляем в этот класс типы, и на данных этого типа мы сможем применять операции которые определены в
-- general классе типов
-- https://stepik.org/lesson/8420/step/7?unit=1556
class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

--
-- реализовать представитель класса типов Printable для пары типа (a,b)
-- https://stepik.org/lesson/8420/step/9?unit=1556
instance (Printable a, Printable b) => Printable (a, b) where {-тут мы требуем, чтобы элементы пары относились к классу типов
Printable, чтобы мы могли совершить на них операцию toString-}
  toString (x,y) = "(" ++ toString x ++ "," ++ toString y ++ ")"
--
-- Задача на расширение класса типов
-- https://stepik.org/lesson/12399/step/3?unit=2829
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where {-расширение классов типов-}
    stompOrStab :: a -> a
    stompOrStab x | doesEnrageGork x && doesEnrageMork x = stomp (stab x)
    stompOrStab x | doesEnrageMork x = stomp x
    stompOrStab x | doesEnrageGork x = stab x
    stompOrStab x | otherwise = x
--
-- Задача: реализовать расширение safeEnum, которое позволит создавать представители класса SafeEnum и осуществлять тотальные
-- операции sssucc и spred
-- https://stepik.org/lesson/12399/step/7?unit=2829
class (Enum a, Eq a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x | x == maxBound = minBound
          | otherwise = succ x

  spred :: a -> a
  spred x | x == minBound = maxBound
          | otherwise = pred x
--
-- Реализовать функцию avg принимающая 3 аргумента типа Int и вычисляющую среднее значение
avg :: Int -> Int -> Int -> Double
avg x y z = fromInteger(toInteger x + toInteger y + toInteger z) / 3
-- Как это все работает:
-- во, первых классное объяснение есть на степике:
-- https://stepik.org/lesson/12399/step/9?discussion=339804&reply=339989&unit=2829
-- работает это так:
-- так как оператор (/) требует тип Fractional, то нам требуется сумму (toInteger x + toInteger y + toInteger z) как-то перевести
-- в тип Fractional
-- окей, напрямую мы скастить к типу Fractional не можем
-- но вот какая штука: функция fromInteger кастит к полиморфному типу (Num a => a), где a - это ЛЮБОЙ тип, реализующий Num
-- более строго: a - это instance тайпкласса Num
-- так вот, этот тип (Num a => a) можно кастить к любому типу, реализующему Num:
-- как пример, пусть у нас есть переменная x = 5 :: (Num a => a)
-- тогда мы можем делать так:
-- y = x :: Int
-- y = x :: Integer
-- y = x :: Double
-- но самое важное, что мы можем кастить это не только в вонкретный тип, а в класс который расширяет Num, и это легально, так как
-- скастив до (Fractional a => a) мы просто убираем некоторое множество инстансов из тайпкласса Num и оставляем возможность типу a
-- быть инстансом тайпкласса Num, которые одновременно являются инстансами тайпкласса Fractional
-- y = x :: (Fractional a => a)
-- итак, что такое y? y теперь имеет тип полиморфный тип (Fractional a => a), а оператор (/) как раз так и требует его
-- что тут означает тип a? a тут - по аналогии с Num a является любым типом, который реализует класс Fractional
-- опять же, a - это instance тайпкласса Fractional

-- то есть если мы имеем полиморфный тип (Num a => a) он может приниматься любыми функциями операторами которые работают с
-- Numeric типами
-- Например, пусть есть такая функия:
getDouble :: Double -> Double
getDouble x = x
-- тогда мы вполне легально можем применить выше приведенную переменную x к функции getDouble:
xPolymorphic = 5 :: (Num a => a)
getDoubleResult = getDouble xPolymorphic {-результат: 5.0-}
-- на этом примере видно, что xPolymorphic просто скастилось в тип Double, так как функция getDouble принимает Double
--
-- еще один пример:
divFract :: Fractional a => a -> a
divFract x = x / 2

-- в такую функцию мы можем опять передать xPolymorphic и получить результат
divFractResult = divFract xPolymorphic {-2.5-}
-- при чем divFractResult будет типа:
-- :t DivFractResult - Double
-- Double было выбрано самой функцией, нам это не важно. Важно то, что (Num a => a) легко превращается в (Fractional a => a)
-- на самом деле для каждого класса определено дефоолтный тип, который выбирается при неоднозначности. Действительно, какой
-- тип присвоить данным типа (Fractional a => a).
-- Хаскель определяет такие дефолтные типы:
-- default Num Integer
-- default Real Integer
-- default Enum Integer
-- default Integral Integer
-- default Fractional Double
-- default RealFrac Double
-- default Floating Double
-- default RealFloat Double
-- что собственно и происходило в выражении fromInteger(toInteger x + toInteger y + toInteger z) / 3
-- Действительно, попробуем вызвать функцию avg:
avgRes = avg 5 6 7
-- :t avgRes
-- avgRes :: Double
-- так как неоднозначен тип суммы, то он просто кастится к дефолтному для Fractional типу - Double
--
-- Посчитать шаги редукции с механизмом разделения
-- https://stepik.org/lesson/8421/step/5?unit=1557

-- Определение функций такое:
bar x y z = x + y
foo a b = bar a a (a + b)
value = foo (3 * 10) (5 - 2)

-- Тогда если вызвана функция value, то происходит следующее:
-- 1) foo (3 * 10) (5 - 2) превращается в
-- bar p1 p1 (p1 + p2), где p1 - указатель на аргумент a, что это есть выражение (3 * 10), p2 - указатель на аргумент b
-- 2) подставляется bar: p1 + p1
-- 3) вычисляется выражение p1 и подставляется сразу оба операнда, а не считаются по отдельности: 30 + 30
-- 4) редуцируется 30 + 30 и получается: 60
--
--
-- Просто для понимания типов:
-- Пусть есть выражение const id
-- тогда применяя эту функцию к любым 2 корректныым аргументам в результате будет не 1 аргумент выдал, а 2
-- например:
-- const id 2 5 <- 5
-- Как это работает:
-- :t const
-- const :: a -> b -> a
-- :t id
-- id :: c -> c
-- тогда, подставляя id в const, тип const как бы становится таким:
-- (c -> c) -> b -> (c -> c)
-- а так как мы передали id, то становится так:
-- b -> (c -> c)
-- что есть b -> c -> c
-- и тогда теперь при передаче двух аргументов будет выдаваться второй аргумент
-- мы можем написать так: const const id и снова вернуться к прошлому типу: с -> b -> c, или что более привычнее a -> b -> a, но
-- это одно и то же
--
-- также следует рассмотреть вопрос, почему при передаче 3 аргументов (const id 2 5), не произошло ошибки,
-- ведь функция const требует всего 2
-- дело в том, что все функции в хаскеле не принимают много аргументов, они принимают всего один аргумент,
-- и возвращают функция одного аргумента
-- так, обработка такого выражения const id 2 5 - это на самом деле:
-- ((const id) 2) 5
-- а так как после обработки аргумента id, у нас произошло преобразование из (a -> b -> a) в (b -> (c -> c)), как было рассмотрено
-- выше, то ошибки не возникает