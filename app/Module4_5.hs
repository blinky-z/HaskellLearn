module Module4_5 where

-- -----------------------
-- 4.5 Рекурсивные типы данных
-- -----------------------
--
-- Рекурсивный тип данных - это тип данных, который в своем конструкторе использует этот же тип дааных
-- Пример рекурсивного типа данных - список:
-- data [] a = [] | a : [a]
-- список имеет 2 конструктора - [] и : (важно не путать конструктор данных [] с типом данных [] a)
-- конструктор [] конструирует пустой список:
-- [] :: [a]
-- конструктор : конструирует не пустой список таким образом:
-- первым элементом он принимает значение типа a, и добавляет его к уже готовому списку, который и имеет тип данных [] a,
-- таким образом используя сам же тип в конструкторе
-- строго говоря, инфиксный конструктор : принимает 2 параметра - первый параметр типа a и второй параметр типа [a]

--
-- https://stepik.org/lesson/7009/step/3?unit=1472
-- Тип List, определенный ниже, эквивалентен определению списков из стандартной библиотеки в том смысле, что
-- существуют взаимно обратные функции, преобразующие List a в [a] и обратно. Реализуйте эти функции.
data List a = Nil | Cons a (List a) deriving Show

fromList :: List a -> [a]
fromList Nil         = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList = foldr Cons Nil

--
-- https://stepik.org/lesson/7009/step/4?unit=1472
-- Рассмотрим еще один пример рекурсивного типа данных:
--
--data Nat = Zero | Suc Nat
-- Элементы этого типа имеют следующий вид: Zero, Suc Zero, Suc (Suc Zero), Suc (Suc (Suc Zero)), и так далее.
-- Таким образом мы можем считать, что элементы этого типа - это натуральные числа в унарной системе счисления.
--
-- Мы можем написать функцию, которая преобразует Nat в Integer следующим образом:
 -- -- fromNat :: Nat -> Integer
-- fromNat Zero = 0
-- fromNat (Suc n) = fromNat n + 1
-- Реализуйте функции сложения и умножения этих чисел, а также функцию, вычисляющую факториал.
data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero    = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc $ toNat $ n - 1

add :: Nat -> Nat -> Nat
add x y = toNat $ fromNat x + fromNat y

mul :: Nat -> Nat -> Nat
mul x y = toNat $ fromNat x * fromNat y

fac :: Nat -> Nat
fac n = toNat $ facHelper $ fromNat n
  where
    facHelper 0 = 1
    facHelper n = n * facHelper (n - 1)

--
-- https://stepik.org/lesson/7009/step/5?unit=1472
-- Тип бинарных деревьев можно описать следующим образом:
--
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--
-- Реализуйте функцию height, возвращающую высоту дерева, и функцию size, возвращающую количество узлов в дереве
-- (и внутренних, и листьев). Считается, что дерево, состоящее из одного листа, имеет высоту 0.
data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _)   = 0
height (Node l r) = 1 + max (height l) (height r)

size :: Tree a -> Int
size (Leaf _)   = 1
size (Node l r) = 1 + size l + size r

--
-- https://stepik.org/lesson/7009/step/6?unit=1472
-- Теперь нам нужно написать функцию avg, которая считает среднее арифметическое всех значений в дереве.
-- И мы хотим, чтобы эта функция осуществляла только один проход по дереву. Это можно сделать при помощи вспомогательной
-- функции, возвращающей количество листьев и сумму значений в них. Реализуйте эту функцию.
--data Tree a = Leaf a | Node (Tree a) (Tree a)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf val)  = (1, val)
    go (Node l r) =
      let (c1, s1) = go l
          (c2, s2) = go r
      in (c1 + c2, s1 + s2)


--
--
-- Исправьте определение функции expand так, чтобы она, используя дистрибутивность (а также, возможно, ассоциативность и
-- коммутативность), всегда возвращала значение, эквивалентное данному и являющееся суммой произведений числовых значений
infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand = until (\x -> x == expand1 x) expand1

expand1 :: Expr -> Expr
expand1 ((e1 :+: e2) :*: e) = expand1 e1 :*: expand1 e :+: expand1 e2 :*: expand e
expand1 (e :*: (e1 :+: e2)) = expand1 e :*: expand e1 :+: expand1 e :*: expand e2
expand1 (e1 :+: e2)         = expand1 e1 :+: expand1 e2
expand1 (e1 :*: e2)         = expand1 e1 :*: expand1 e2
expand1 e                   = e
