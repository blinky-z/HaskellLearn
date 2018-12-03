module Module1 where

-- определение своих операторов
infixl 6 *+*

a *+* b = a^2 + b^2 {-оператор определен в инфиксном виде-}
--(*+*) a b = a^2 + b^2 {-определение в префиксном виде, т.е. функциональном-}
--Использование:
s1 = 3 *+* 5 {-инфиксное использование-}
s2 = (*+*) 3 5 {-префиксное-}

--
--приоритет и ассоциативность операторов
--(++) [1,2] 3 : [4,5,6] {-такое работать не будет, потому что приоритет функций равен 10, а вызов функций левоассоциативен
-- функция ожидает 2 списка, первое что она видит - [1,2], удовлетворяет, а вот аргумент 3 не удовлетворяет, и вообще
-- тут 4 аргумента-}
-- еще один пример: (++) [1,2] (:) 3 [4,5,6] - здесь снова вызов (++) видит [1,2] и далее оператор (:), который не подходит
-- исправить первое можно так: (++) [1,2] (3 : [4,5,6])
-- или так: (++) [1,2] $ 3 : [4,5,6]
-- а второе так: (++) [1,2] ((:) 3 [4,5,6])
-- или так: (++) [1,2] $ (:) 3 [4,5,6]
-- а вот если не применять оператор (++) в префиксном стиле, то такое сработает:
-- [1,2] ++ 3 : [4,5,6] {-оба операторы правоассоциативны и имеют одинаковый приоритет, сначала будет вычислено 3 : [4,5,6]
-- и выражение превратится в такое: [1,2] ++ [3,4,5,6]-}
-- также будет работать и такое:
-- [1,2] ++ (:) 3 [4,5,6] - здесь оператор ++ вызван в инфиксном стиле, а определение оператора (++) такое:
-- :i (++)
--infixr 5 ++
-- т.е. сначала вычисление аргументов происхоидт справа: сначала (:) 3 [4,5,6] превращается в [3,4,5,6] и списки успешно
-- складываются
--

-- двойной факториал (задание)
doubleFact :: Integer -> Integer
doubleFact 2 = 2
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)
--
-- фибоначчи через рекурсию самой себя
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)
--
-- факториал через использование цикла использование дополнительной переменной, накапливающей текущее состояние факториала
-- на самом деле используется дополнительная функция, и ее аргумент хранит текущее состояние факториала
factorial2 n | n >= 0 = factorial2Helper 1 n
             | otherwise = error "asg must be >= 0"

factorial2Helper acc 0 = acc
factorial2Helper acc n = factorial2Helper (acc * n) (n - 1)
-- факториал через вызов самого себя
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- определение через аккумулятор - эффективнее. Оптимизация компилятора, оптимизурующая рекурсию таким образом (с применением
-- аккумулятора) называются Tail Call Optimization
--
-- фибоначчи через функцию хелпер (задание) https://stepik.org/lesson/8413/step/10?unit=1552
fibonacci2 :: Integer -> Integer
fibonacci2 n | n >= 0 = fibonacci2Helper1 0 1 (n - 1) + fibonacci2Helper1 0 1 (n - 2)
             | otherwise = fibonacci2Helper1 0 1 (n + 2) - fibonacci2Helper1 0 1 (n + 1)

fibonacci2Helper1 :: Integer -> Integer -> Integer -> Integer
fibonacci2Helper1 acc1 acc2 0 = acc1
fibonacci2Helper1 acc1 acc2 n | n > 0 = fibonacci2Helper1 (acc1 + acc2) acc1 (n - 1)
fibonacci2Helper1 acc1 acc2 n | n < 0 = fibonacci2Helper1 (acc2 - acc1) acc1 (n + 1)
--
-- Рекуррентная последовательность вида a(k+3)=a(k+2)+a(k+1)−2ak (задача) https://stepik.org/lesson/8414/step/6?unit=1553
--с применением локального связывания. Функция seqAHelper видна только в области видимости функции seqA
--также сделана оптимизация рекурсии - используется аккумулятор, и кол-во вызовов функций линейно
seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = seqAHelper 1 2 3 (n - 1) + seqAHelper 1 2 3 (n - 2) - 2 * seqAHelper 1 2 3 (n - 3)
  where
    seqAHelper acc1 acc2 acc3 0 = acc1
    seqAHelper acc1 acc2 acc3 1 = acc2
    seqAHelper acc1 acc2 acc3 2 = acc3
    seqAHelper acc1 acc2 acc3 n = seqAHelper acc2 acc3 (acc3 + acc2 - 2 * acc1) (n - 1)
--
-- Найти сумму цифр числа и кол-во цифр https://stepik.org/lesson/8414/step/8?unit=1553
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x < 0 = sum'n'count (x * (-1))
              | otherwise = (sumDigits x, countDigits x)
  where
    sumDigits :: Integer -> Integer
    sumDigits x = sumDigitsHelper 0 x

    sumDigitsHelper acc x | x < 10 = acc + x
                          | otherwise = sumDigitsHelper (acc + x `mod` 10) (x `div` 10)

    countDigits :: Integer -> Integer
    countDigits x = countDigitsHelper 1 (x `div` 10)

    countDigitsHelper acc 0 = acc
    countDigitsHelper acc x = countDigitsHelper (acc + 1) (x `div` 10)
--
--Найти определенный интеграл методом трапеций. Кол-во отрезков = 1000 https://stepik.org/lesson/8414/step/9?unit=1553
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b | a > b = -integration f b a
                  | otherwise = h * (((f a + f b) / 2) + integrationHelper 0 (a + h) 1)
  where
    h = (b - a) / 1000
    integrationHelper :: Double -> Double -> Int -> Double
    integrationHelper acc x i | i == 1000 = acc
                              | otherwise = integrationHelper (acc + f x) (x + h) (i + 1)
--