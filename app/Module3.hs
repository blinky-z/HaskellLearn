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
isPalindrome (x:list) | x == head (reverse list) = isPalindrome (reverse (tail (reverse list)))
                      | otherwise = False

isPalindrome' :: Eq a => [a] -> bool
isPalindrome' xs = xs == reverse xs