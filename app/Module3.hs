module Module3 where

import Data.Char

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

--
-- Предположим, что функция
   --repeat
   --, была бы определена следующим образом:
   --repeat = iterate repeatHelper
   --определите, как должна выглядеть функция
   --repeatHelper
repeatHelper =