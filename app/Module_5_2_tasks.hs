module Module_5_2_tasks where

-- Введём следующий тип:
data Log a = Log [String] a deriving Show
-- Реализуйте вычисление с логированием, используя Log. Для начала определите функцию toLogger
--toLogger :: (a -> b) -> String -> (a -> Log b)

-- которая превращает обычную функцию, в функцию с логированием:
-- Далее, определите функцию execLoggers
--execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
-- Которая принимает некоторый элемент и две функции с логированием.
-- execLoggers возвращает результат последовательного применения функций к элементу и список сообщений, которые были выданы
-- при применении каждой из функций
toLogger :: (a -> b) -> String -> a -> Log b
toLogger f msg v = Log [msg] (f v)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g =
  let Log m1 y = f x
      Log m2 z = g y
  in Log (m1 ++ m2) z

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

-- итак, это есть монада, которая добавляет дополнительный эффект на вычисления:
-- кроме того, что мы с помощью функции f вычисляем какое-то значение типа b, мы также вместе с этим значением возвращает
-- запись лога
-- то есть мы можем добавить к проделанным вычислениям комментарии для записи в лог

--
-- https://stepik.org/lesson/8437/step/5?unit=1572
returnLog :: a -> Log a
returnLog = Log []

-- данная функция - это реализация функции return для контейнера Log

--
-- https://stepik.org/lesson/8437/step/7?unit=1572
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log xs x) f =
  let (Log ys y) = f x
  in Log (xs ++ ys) y

-- данная функция - ведет себя эквивалентно (>>=)
-- достает значение из передаваемого контейнера, обрабатывает его функцией и возвращает новый контейнер с обработанным значением
-- при этом реализация такова, что мы сохраняем прошлый контекст, то есть все старые записи лога сохраняются

--
-- https://stepik.org/lesson/8437/step/8?unit=1572
instance Functor Log where

instance Applicative Log where

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x fs = foldl (>>=) (return x) fs

-- идея такая: начинаем идти сначала и применять оператор (>>=) по очереди к результату слева и функции справа,
-- функция будет доставать значение и контекст с операнда слева и строить новый контейнер
-- инициализирующий элемент - Log с пустыми сообщениями и передающимся в функцию execLoggersList начальным значением x