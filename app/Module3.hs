module Module3 where

-- Работа со списками

-- Реализуйте функцию addTwoElements, которая бы добавляла два переданных ей значения в голову переданного списка.
-- https://stepik.org/lesson/8326/step/3?unit=1474
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements arg1 arg2 list = arg1 : arg2 : list

--
--
-- Реализуйте функцию nTimes, которая возвращает список, состоящий из повторяющихся значений ее первого аргумента.
-- Количество повторов определяется значением второго аргумента этой функции.
nTimes:: a -> Int -> [a]
nTimes x n = nTimesHelper 0 []
  where
    nTimesHelper cnt list | cnt == n = list
                          | otherwise = nTimesHelper (cnt + 1) (x : list)

--
--
-- Сформируйте список целых чисел, содержащий только те элементы исходного списка, значение которых нечетно.
oddsOnly :: Integral a => [a] -> [a]
oddsOnly xs = oddsOnlyHelper xs []
  where
    oddsOnlyHelper [] ans = reverseList [] ans
    oddsOnlyHelper (x:xs) ans | odd x = oddsOnlyHelper xs (x : ans)
                              | otherwise = oddsOnlyHelper xs ans
    reverseList rxs [] = rxs
    reverseList rxs (x:xs) = reverseList (x:rxs) xs

--
--
-- Реализуйте функцию isPalindrome, которая определяет, является ли переданный ей список палиндромом.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome [x,y] = x == y
isPalindrome (x:list) | x == last list = isPalindrome (init list)
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
    sum3Helper res curn | curn == maxn = reverse res
                        | otherwise = sum3Helper ((getElem xs curn 0 + getElem ys curn 0 + getElem zs curn 0) : res) (curn + 1)

    getElem [] n curn = 0
    getElem (x:xs) n curn | curn == n = x
                          | otherwise = getElem xs n (curn + 1)

--
--
-- Напишите функцию groupElems которая группирует одинаковые элементы в списке
-- https://stepik.org/lesson/8326/step/13?unit=1474
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems xs = groupElemsHelper xs [] []
  where
    groupElemsHelper [x] curlist res
      | null curlist = reverse ([x] : res)
      | x == last curlist = reverse (curlist : res)
      | otherwise = reverse ([x] : res)
    groupElemsHelper (prev:xs) curlist res
      | null curlist = groupElemsHelper (prev:xs) (prev : curlist) res
      | head xs == prev = groupElemsHelper xs (head xs : curlist) res
      | otherwise = groupElemsHelper xs [] (curlist : res)

--
--
--