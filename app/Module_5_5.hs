module Module_5_5 where

import Control.Monad

--
-- -----------------------
-- 5.5 Монада IO
-- -----------------------
--

-- В хаскеле IO реализован с помощью монады IO

main = do
  putStrLn "What is your Name?"
  name <- getLine
  putStrLn $ "Nice to meet you, " ++ name ++ "!"

-- ввод-вывод сделан в хаскеле с помощью монадического вычисления и только в монадическом вычислении
-- весь ввод-вывод упаковывается в контейнер IO, и с помощью <- можно доставать ввод из монады IO
-- также, можно посмотреть тип main:
-- :t main
--  -> main :: IO ()
-- мы видим, что монада параметризована типом (), это называется unit type, пустое значение, null
-- так как функция main не возвращает никакое значение и мы не ожидаем от нее никакой информации о возвращаемом значении,
-- можно считать это как void
-- такой тип следует из типа функции putStrLn:
--  -> putStrLn :: String -> IO ()
-- putStrLn - это Стрелка Клейсли, которая упаковывает значение в контейнер
-- есть еще функция для ввода - getLine
-- ее тип:
-- :t getLine
--  -> getLine :: IO String
-- данная функция упаковывает строку с ввода в контейнер IO, параметрзованный String, и можем достать значение в переменную

-- почему вообще надо упаковывать ввод-вывод в монаду IO?
-- в хаскеле это сделано из-за гарантий, предоставляемых чистыми функциями
-- например, пусть у нас есть функция getCharFromConsole :: Char
-- такая функция не должна возвращать никогда разные значения, так как там даже нет ветвлений
-- такая функция всегда должна возвращать константу, в то время как ввод может быть разным

-- идея ввода-вывода в хаскеле реализована следующим способом:
-- newtype IO a = IO (RealWorld -> (RealWorld, a))
-- RealWorld представляет собой абстракцию, которая взаимодействует с внешним миром
-- тип IO хранит внутри себя функцию, которая преобразует внешний мир, на котором пользователь готовится набирать что-то,
-- в внешний мир, на котором уже нажата клавиша, и заносит ввод в 2 элемент пары (RealWorld, a)
--
-- хранить состояние нового мира (1 элемент пары) необходимо для того, чтобы осуществить следующий ввод, так как если не знать,
-- где мы остановились читать, то непонятно откуда и начинать читать следующий ввод
-- осуществлять ввод-вывод
--
-- можно подумать, что компилятору придется часто доставать и помещать значение в монаду, но на самом деле, нет
-- потому что newtype - это всего лишь обертка над существующим элементом и 1 параметром, как мы говорили ранее, поэтому
-- на самом деле в рантайме не будет никаких конструкторов данных

-- напишем реализацию инстанса Монады для типа IO:
{-
newtype IO a = IO (RealWorld -> (RealWorld, a))

return :: a -> IO a

(>>=) :: (RealWorld -> (RealWorld, a)) -> (a -> (RealWorld -> (RealWorld, b))) -> RealWorld -> (RealWorld, b)

instance Monad IO where
  return a = \w -> (w, a)

  (>>=) m k = \w -> case m w of (w', a) -> k a w'
-}

-- идея оператора (>>=) такова:
-- в монаду IO, которая ожидает внешнего мира, на котором ожидается ввод, передается внешний мир и монада с помощью
-- внутренней функции, получает введенное значение с переданного внешнего мира, и строит новый внешний мир
-- pattern matching тут нужен, так как мы хотим разобрать результат работы функции монады IO
-- ну и дальше как и требуется, на значение контейнера применяется функция Клейсли и туда передается новый внешний мир

-- существует функции, облегчающие работу с монадами, которые доступны из модуля Control.Monad

-- первая - это sequence_
-- данная функция вычисляет последовательно все инструкции, но игнорирует полученное значение
{-
sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())
-}
-- использование:
-- sequence_ [Just 1, Just 2]
--  -> Just ()
-- для чего это могло быть использовано, просто чтобы произвести при вычислении какой-то эффект, здесь - Nothing
-- sequence_ [Just 1, Nothing]
--  -> Nothing (потому что для Maybe операторы (>>) и (>>=) работают так:
-- Nothing >> _ = Nothing
-- Nothing >>= _ = Nothing)
-- sequence_ [[1,2],[3,4,5,6]]
--  -> [(),(),(),(),(),(),(),()]
-- sequence_ [putChar 'a', putChar 'b']
--  -> "ab"
-- sequence_ $ map putChar "ab" <- тут важно понимать то, что putChar не будет исполнена, а просто положена в список, так как
-- putChar - производит только монаду, а чтобы производить вычисление с монадой, нам требуется собественно цепочка монадических
-- вычислений
--  -> "ab"

-- mapM
-- данная функция принимает стрелку Клейсли и на каждом элементе массива применяет эту стрелку, порождая монады, а потом
-- производит цепочку монадических вычислений, игнорирую результат
{-
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ . map f
-}

-- собственно, тогда такое (sequence_ $ map putChar "ab") можно написать так:
-- mapM_ putChar "ab"

-- теперь, функции, которые возвращают результат цепочки вычислений:
-- sequence
-- данная функция вычисляет все элементы и возвращает результат работы цепочки вычислений, упакованный в ту же монаду
{-
sequence :: Monad m => [m a] -> m [a]
sequence ms = foldr k (return []) ms
  where
    k :: Monad m => m a -> m [a] -> m [a]
    k m m' = do
      x <- m
      xs <- m'
      return (x:xs)
-}
-- использование:
-- sequence [Just 1, Just 2]
--  -> Just [1,2]
-- sequence [getLine, getLine]
-- :t [getLine, getLine] -- [getLine, getLine] :: [IO String]
-- :t sequence [getLine, getLine] - это IO [String]
-- ввод: ab
-- ввод: cde
--  -> ["ab","cde"]

-- mapM, то же самое что и mapM_, но результат сохраняется
{-
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f
-}