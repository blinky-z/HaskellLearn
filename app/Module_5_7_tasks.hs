module Module_5_7_tasks where

import           Data.Monoid

newtype Writer w a = Writer {runWriter :: (a, w)} deriving Show

instance (Monoid w) => Functor (Writer w)

instance (Monoid w) => Applicative (Writer w)

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  m >>= k =
    let (x, u) = runWriter m
        (y, v) = runWriter $ k x
    in Writer (y, u `mappend` v)

tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

--
-- https://stepik.org/lesson/8442/step/3?unit=1577
evalWriter :: Writer w a -> a
evalWriter m = fst (runWriter m)

--
-- https://stepik.org/lesson/8442/step/6?unit=1577
type Shopping = Writer (Sum Integer) () {-синоним типа, не конструктор данных-}

purchase :: String -> Integer -> Writer (Sum Integer) ()
purchase _ cost = tell (Sum cost)

total :: Shopping -> Integer
total m = getSum $ execWriter m

--
-- https://stepik.org/lesson/8442/step/7?unit=1577
-- пара моноидов на самом деле тоже моноид:  instance (Monoid a, Monoid b) => Monoid (a, b)
-- поэтомы мы можем использовать в качестве лога пару моноидов для решения задачи
--
type Shopping' = Writer (Sum Integer, [String]) ()

purchase' :: String -> Integer -> Shopping'
purchase' item cost = tell (Sum cost, [item])

total' :: Shopping' -> Integer
total' m = case execWriter m of
  (sum, _) -> getSum $ sum

items' :: Shopping' -> [String]
items' m = case execWriter m of
  (_, xs) -> xs
