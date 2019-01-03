module Module3 where

import Data.Char
import Data.List

-- Работа со списками
-- Реализуйте функцию addTwoElements, которая бы добавляла два переданных ей значения в голову переданного списка.
-- https://stepik.org/lesson/8326/step/3?unit=1474
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements arg1 arg2 list = arg1 : arg2 : list

--
--
-- Реализуйте функцию nTimes, которая возвращает список, состоящий из повторяющихся значений ее первого аргумента.
-- Количество повторов определяется значением второго аргумента этой функции.
nTimes :: a -> Int -> [a]
nTimes x n = nTimesHelper 0 []
  where
    nTimesHelper cnt list
      | cnt == n = list
      | otherwise = nTimesHelper (cnt + 1) (x : list)

--
--
-- Сформируйте список целых чисел, содержащий только те элементы исходного списка, значение которых нечетно.
oddsOnly :: Integral a => [a] -> [a]
oddsOnly xs = oddsOnlyHelper xs []
  where
    oddsOnlyHelper [] ans = reverseList [] ans
    oddsOnlyHelper (x:xs) ans
      | odd x = oddsOnlyHelper xs (x : ans)
      | otherwise = oddsOnlyHelper xs ans
    reverseList rxs [] = rxs
    reverseList rxs (x:xs) = reverseList (x : rxs) xs

--
--
-- Реализуйте функцию isPalindrome, которая определяет, является ли переданный ей список палиндромом.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome [x, y] = x == y
isPalindrome (x:list)
  | x == last list = isPalindrome (init list)
  | otherwise = False

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' xs = xs == reverse xs

--
--
-- Составьте список сумм соответствующих элементов трех заданных списков
-- https://stepik.org/lesson/8326/step/12?unit=1474
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = sum3Helper [] 0
  where
    maxn = max (max (length xs) (length ys)) (length zs)
    sum3Helper res curn
      | curn == maxn = reverse res
      | otherwise = sum3Helper ((getElem xs curn 0 + getElem ys curn 0 + getElem zs curn 0) : res) (curn + 1)
    getElem [] n curn = 0
    getElem (x:xs) n curn
      | curn == n = x
      | otherwise = getElem xs n (curn + 1)

--
--
-- Напишите функцию groupElems которая группирует одинаковые элементы в списке (если они идут подряд)
-- https://stepik.org/lesson/8326/step/13?unit=1474
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems xs = ys : groupElems zs
  where
    (ys, zs) = span (== head xs) xs

--
--
-- Функции высших порядков над списками
--
-- Напишите функцию
-- readDigits
-- ,принимающую строку и возвращающую пару строк.
-- Первый элемент пары содержит цифровой префикс исходной строки, а второй - ее оставшуюся часть.
-- https://stepik.org/lesson/12321/step/3?unit=2785
readDigits :: String -> (String, String)
readDigits = span isDigit

--
-- Реализуйте функцию
-- filterDisj
-- , принимающую два унарных предиката и список, и возвращающую список элементов, удовлетворяющих хотя бы одному из предикатов.
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj pred1 pred2 xs = filterDisjHelper xs []
  where
    filterDisjHelper (x:xs) res
      | pred1 x = filterDisjHelper xs (x : res)
      | pred2 x = filterDisjHelper xs (x : res)
      | otherwise = filterDisjHelper xs res
    filterDisjHelper [] res = reverse res

filterDisj' :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj' pred1 pred2 = filter (\x -> pred1 x || pred2 x)

--
-- Напишите реализацию функции qsort.
-- Функция qsort должная принимать на вход список элементов и сортировать его в порядке возрастания с помощью сортировки Хоара:
-- для какого-то элемента x изначального списка (обычно выбирают первый) делить список на элементы меньше и не меньше x, и потом
-- запускаться рекурсивно на обеих частях.
qsort :: Ord a => [a] -> [a]
qsort [x] = [x]
qsort [] = []
qsort xs'@(x:xs) = qsort (filter (< x) xs') ++ filter (== x) xs' ++ qsort (filter (> x) xs')

--
-- Напишите функцию
-- squares'n'cubes
-- , принимающую список чисел,
-- и возвращающую список квадратов и кубов элементов исходного списка.
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

--
-- Воспользовавшись функциями map и concatMap, определите функцию perms, которая возвращает все перестановки,
-- которые можно получить из данного списка, в любом порядке.
perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = permsHelper xs
  where
    permsHelper [x] = [[x]]
    permsHelper (x:xs) =
      let list = permsHelper xs
       in concatMap (insertInAllPos x [] 0) list
    insertInAllPos val res curn xs
      | curn > n = res
      | otherwise = insertInAllPos val (insertAt xs val curn : res) (curn + 1) xs
      where
        n = length xs
    insertAt xs val pos = ys ++ [val] ++ zs
      where
        (ys, zs) = splitAt pos xs

-- более крутое решение, взял с решений на степике. Идея та же, но реализация круче
perms' :: [a] -> [[a]]
perms' [] = [[]]
perms' [x] = [[x]]
perms' (x:xs) = concatMap (insertElem x) (perms xs)
  where
    insertElem x [] = [[x]]
    insertElem x yss@(y:ys) = (x : yss) : map (y :) (insertElem x ys)

--
--
-- Реализуйте функцию delAllUpper, удаляющую из текста все слова, целиком состоящие из символов в верхнем регистре.
-- Предполагается, что текст состоит только из символов алфавита и пробелов, знаки пунктуации, цифры и т.п. отсутствуют.
delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

--
--
-- Напишите функцию max3, которой передаются три списка чисел одинаковой длины и которая возвращает список чисел той же длины,
-- содержащий на k-ой позиции наибольшее значение из чисел на этой позиции в списках-аргументах.
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> max (max x y) z)

--
-- Генераторы списков
--
-- Реализуйте c использованием функции zipWith функцию fibStream, возвращающую бесконечный список чисел Фибоначчи.
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
  -- Предположим, что функция
   --repeat
   --, была бы определена следующим образом:
   --repeat = iterate repeatHelper
   --определите, как должна выглядеть функция
   --repeatHelper

--
repeatHelper x = x

--
-- https://stepik.org/lesson/8328/step/7?unit=1476
data Odd =
  Odd Integer
  deriving (Eq, Show)

instance Enum Odd where
  succ (Odd x) = Odd (x + 2)
  pred (Odd x) = Odd (x - 2)
  toEnum x = Odd (toInteger x)
  fromEnum (Odd x) = fromInteger x
  enumFrom (Odd x) = Odd x : enumFrom (succ (Odd x))
  enumFromThen (Odd x) (Odd y) = worker (Odd x) (Odd (y - x))
    where
      worker (Odd x) (Odd y) = Odd x : worker (Odd (x + y)) (Odd y)
  enumFromTo (Odd x) (Odd y) = worker (Odd x) (Odd y)
    where
      worker (Odd x) (Odd y)
        | x == y = [Odd x]
        | x > y = []
        | otherwise = Odd x : worker (succ (Odd x)) (Odd y)
  enumFromThenTo (Odd x1) (Odd x2) (Odd y)
    | x2 >= x1 = workerUp (Odd x1) (Odd (x2 - x1)) (Odd y)
    | otherwise = workerDn (Odd x1) (Odd (x2 - x1)) (Odd y)
    where
      workerUp (Odd x) (Odd d) (Odd y)
        | x == y = [Odd x]
        | x > y = []
        | otherwise = Odd x : workerUp (Odd (x + d)) (Odd d) (Odd y)
      workerDn (Odd x) (Odd d) (Odd y)
        | x == y = [Odd x]
        | x < y = []
        | otherwise = Odd x : workerDn (Odd (x + d)) (Odd d) (Odd y)

--
--Пусть есть список положительных достоинств монет
--coins
--, отсортированный по возрастанию. Воспользовавшись механизмом генераторов списков, напишите функцию
--change
--, которая разбивает переданную ей положительную сумму денег на монеты достоинств из списка
--coins
--всеми возможными способами. Например, если
--coins = [2, 3, 7]
--
--
--
--GHCi> change 7
--[[2,2,3],[2,3,2],[3,2,2],[7]]
--
--Примечание. Порядок монет в каждом разбиении имеет значение, то есть наборы [2,2,3] и [2,3,2] — различаются.
--Список coins определять не надо.
-- динамическое программирование:
-- если мы начали с какой-то монеты, и постепенно добавляли новые монеты и при добавлении новых монет сумма не становилась
-- меньше нуля, а в какой-то момент стала равной нулю, то это значит, что мы собрали такой массив, монеты в котором дают
-- ровно заданное число, и дополнительной проверки (x + sum xs == c) делать не надо
-- если на каком то шаге сумма стала меньше нуля, то значит мы добрали неподходящую монету, и ответа с таким набором нет
-- тогда тут: xs <- change $ c - x, будет воззвращено [], и ответа просто нет, далее берется новый coin и опять запускаем с уже
-- другой монетой xs <- change $ c - x
-- если на этом шаге вообще не получилось добрать правильной монеты, то мы возвращаемся на шаг назад (ведь следующий шаг
-- был инициирован предыдущим шагом вызовом xs <- change $ c - x, и при возвращении берется новая монета и снова запускается
-- change, и так пока не соберем правильный массив или вообще нет ответа
change :: (Ord a, Num a) => a -> [[a]]
change c
  | c < 0 = []
  | c == 0 = [[]]
  | otherwise = [x : xs | x <- [2, 3, 7], xs <- change $ c - x] {-coins-}

--
-- Правая свертка
--
-- реализация:
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f init [] = init
-- foldr f init (x:xs) = x `f' foldr f init xs
-- что это такое:
-- обобщает функции вида x <binOp> xs, где x - элемент списка, xs - оставшая часть списка
-- первый аргумент функции - это binOp, бинарная операция которая применяется к частям списка
-- первым аргументом операции является элемент списка типа [a], значит тип первого аргумента - a
-- вторым аргументом является вызов функции foldr над оставшейся частью списка, а так как функция возвращает тип b, то и
-- второй аргумент имеет тип b
-- возвращаемое бинарной операцией значение имеет тип b, так как это есть значение вызова самой функции
-- вторым аргументом функции является терминирующее значение, значение, которое выдается функцией на пустом списке.
-- Терминирующее значение должно быть того же типа, что и возввращаемое функцией значение
-- третьим аргументом является сам список типа [a], над которым совершается функция
-- например:
-- sumList :: [Integer] -> Integer
-- sumList [] = 0
-- sumList (x:xs) = x + sumList xs
-- через foldr:
-- sumList = foldr (+) 0
-- задачи
-- Напишите реализацию функции concatList через foldr
concatList :: [[a]] -> [a]
concatList = foldr (++) []

--
-- Используя функцию foldr, напишите реализацию функции lengthList, вычисляющей количество элементов в списке.
lengthList :: [a] -> Int
lengthList = foldr f 0
  where
    f x s = 1 + s

--
-- Реализуйте функцию sumOdd, которая суммирует элементы списка целых чисел, имеющие нечетные значения:
sumOdd :: [Integer] -> Integer
sumOdd =
  foldr
    (\x s ->
       if odd x
         then x + s
         else s)
    0

--
-- почему правая свертка так названа
-- потому что действия начинают выполняться справа, то есть конструкция все расширяется и расширяется, пока не будет
-- достигнуто терминирующее условие, и список начнет собираться с конца
-- например:
-- foldr f init [1,2,3] превратится в:
-- 1 `f` (2 `f` (3 `f` init))
--
-- Какой функции стандартной библиотеки, суженной на списки, эквивалентно выражение foldr (:) []?
-- Ответ: id
-- Какой функции стандартной библиотеки эквивалентно выражение foldr const undefined?
-- Ответ: head
--
-- Левая свертка
--
-- реализация:
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f init [] = init
-- foldl f init (x:xs) = foldl f (f init x) xs
--
-- Левая свертка отличается тем, что действия начинают выполняться сначала, начиная с иницииализирующего значения
-- например:
-- foldl f init [1,2,3] превратится в:
-- ((init `f` 1) `f` 2) `f` 3
-- или в функциональном стиле: f (f (f init 1) 2) 3
-- по шагам:
-- foldl f init 1:2:3:[]
-- -> foldl f (f init 1) (2:3:[])
-- -> foldl f (f (f init 1) 2) (3:[])
-- -> foldl f (f (f (f init 1) 2) 3) []
-- -> f (f (f init 1) 2) 3
-- ((init `f` 1) `f` 2) `f` 3
--
-- в типе определения меняется только тип сворачивающей функции
-- так как первым операндом сворачивающей функцией теперь является результат вычисления первых элементов списка, а потом
-- только элементы списка, то первый операнд имеет тип b, т.е. результат вычисления сворачивающей функции, которая и вовзращает
-- значение типа b, а второй операнд тип a, т.е. элемент списка типа [a]
--
-- При каком значении переменной x следующие два выражения примут одно и то же значение (отличное от неопределенного)?
-- Ответ: 7
-- левая свертка неэффективна, так как постоянно создается много thunk-ов, а именно (f init x)
-- строгая реализация левой свертки
-- foldl' :: (b -> a -> b) -> b -> [a] -> b
-- foldl' f init [] = init
-- foldl' f init (x:xs) = init' `seq` foldl f init' xs
--    where init' = f init x
-- такая реализация отличается тем, что нет отложенных вычислений, и тратится меньше памяти
-- также, почему-то строгая реализация левой свертки работает намного быстрее обычной левой свертки, не понял почему
-- по сути, дело должно было быть только в памяти, но еще и становится эффективнее по времени строгая реализация
-- разберем операцию + с элементами списка в 3 функциях: foldr, foldl, foldl'
-- foldr (+) 0 $ take 100000000 [1..]
-- *** Exception: stack overflow
-- так происходит, из-за реализации функции foldr, если посмотреть, можно увидеть, что ей приходится накапливать результат вычислений
-- прошлого шага: x `f' foldr f init xs <- требуется хранить x
-- foldl (+) 0 $ take 100000000 [1..]
-- нет ошибки stack overflow, так как на функции может быть выполнена tail call optimization, но зато просто виснет система, из-за того
-- что приходится хранить много отложенных вычислений, а именно
-- (f init x): foldl f (f init x) xs
-- foldl' (+) 0 $ take 100000000 [1..]
-- отработала легко, так как во первых, отложеннных вычислений нет, и на функции может быть выполнена оптимизация хвостовых вызовов
--
--
-- Реализуйте функцию meanList, которая находит среднее значение элементов списка, используя однократный вызов функции свертки.
meanList :: [Double] -> Double
meanList = meanListHelper . foldr (\x (s, l) -> (s + x, l + 1)) (0, 0)
  where
    meanListHelper :: (Double, Double) -> Double
    meanListHelper (s, l) = s / l

--
-- Используя однократный вызов свертки, реализуйте функцию evenOnly, которая выбрасывает из списка элементы,
-- стоящие на нечетных местах, оставляя только четные.
evenOnly :: [a] -> [a]
evenOnly xs = evenOnlyHelper (foldr (\x ~(l, (ys1, ys2)) -> if even l then (l+1,(x:ys1, ys2)) else (l + 1, (ys1, x:ys2))) (0, ([],[])) xs)
  where
    evenOnlyHelper ~(l, (xs,ys)) | odd l = ys
                                 | otherwise = xs

--
-- Попробуйте добиться того, чтобы реализованная вами в прошлом задании функция
-- evenOnly
-- позволяла работать и с бесконечными списками.

-- прошлая функция не работала из-за того, что хоть и есть lazy pattern matching, но требуется использовать l в выражение if even l,
-- поэтому функция будет пытаться вычислить l до бексонечности
-- в этом решении та же самая идея, что и в прошлом решении (брать по очереди элементы в разные списки), просто без счетчика
evenOnly' :: [a] -> [a]
evenOnly' xs = snd (foldr (\x ~(ys, zs) -> (x : zs, ys)) ([], []) xs)